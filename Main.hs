module Main where

import IdeSlave (SExp(..), parseSExp, convSExp)
import IrcColor

import Control.Concurrent (forkIO, myThreadId, throwTo, ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, takeMVar, tryTakeMVar, putMVar, readMVar, tryPutMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (replicateM, forM_)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.List (stripPrefix, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Network.SimpleIRC
import Numeric (readHex)
import System.Directory (createDirectory, getTemporaryDirectory, doesFileExist, doesDirectoryExist, copyFile, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath ((</>), (<.>))
import System.IO (Handle, hGetChar, hPutStrLn, hSetBuffering, BufferMode(..), hClose)
import System.Process (proc, CreateProcess(..), StdStream(..), createProcess, readProcess, ProcessHandle, terminateProcess)
import System.Time (ClockTime(..), getClockTime)
import System.Timeout (timeout)

decodeUTF8 :: ByteString -> String
decodeUTF8 = unpack . decodeUtf8With lenientDecode

encodeUTF8 :: String -> ByteString
encodeUTF8 = encodeUtf8 . pack

type RetRepo = MVar (Integer, Map Integer (ByteString, MVar ()))

setTimeout :: Int -> ThreadId-> IO (MVar ())
setTimeout n tid = do
  canceler <- newEmptyMVar
  _ <- forkIO $ do
    m <- timeout n $ takeMVar canceler
    case m of
      Just _ -> return ()
      Nothing -> throwTo tid $ userError "timeout"
  return canceler

sendQuery :: Handle -> String -> Integer -> IO ()
sendQuery h q i = hPutStrLn h $ convSExp "interpret" q i

firstWordIs :: String -> String -> Bool
firstWordIs wd str = case words str of
    [] -> False
    (w:_) -> w == wd

allowedCommands :: [String]
allowedCommands = [ "t"
                  , "type"
                  , "i"
                  , "info"
                  , "doc"
                  , "total"
                  , "apropos"
                  ]

filterQuery :: String -> Maybe String
filterQuery q = case dropWhile isSpace q of
  ':':cs | any (`firstWordIs` cs) allowedCommands -> Nothing
         | otherwise -> Just "Command not permitted"
  _ -> Nothing

onMessage :: ThreadId -> RetRepo -> Handle -> EventFunc
onMessage mainth r h mirc msg = case asum . map (`stripPrefix` decodeUTF8 (mMsg msg)) $ ["> ","( ", "idris-ircslave: "] of
                                 Nothing -> return ()
                                 Just query -> case mOrigin msg of
                                   Nothing -> return ()
                                   Just origin -> case filterQuery query of
                                     Just err -> sendMsg mirc origin . encodeUTF8 $ err
                                     Nothing -> do
                                       (i, m) <- takeMVar r
                                       sendQuery h query i
                                       canceler <- setTimeout (10 * 10 ^ (6 :: Int)) mainth
                                       putMVar r (i + 1, Map.insert i (origin,canceler) m)
                                `catch` (throwTo mainth :: SomeException -> IO ())

readResp :: Handle -> IO SExp
readResp h = do
  hx <- replicateM 6 $ hGetChar h
  case readHex hx of
    ((n, ""):_) -> do
      sex <- replicateM n $ hGetChar h
      case parseSExp sex of 
        Right r -> return r
        e -> error $ "unexpected parse: " ++ show e
    _ -> error "desynced from idris output"

data Decor = Type | Data | Function | Bound Bool -- is it implicit?
  deriving (Show, Read, Eq)

readDecor :: String -> Maybe Decor
readDecor "type" = Just Type
readDecor "data" = Just Data
readDecor "function" = Just Function
readDecor "bound" = Just (Bound False)
readDecor _ = Nothing

decorSpan :: SExp -> Maybe (Integer,Integer,Decor)
decorSpan (SexpList [IntegerAtom start, IntegerAtom len, SexpList annotations]) = do
  decorstring <- listToMaybe [ decor | SexpList [SymbolAtom "decor", SymbolAtom decor] <- annotations ]
  decor <- readDecor decorstring
  case decor of
    Bound _ | Just True <- listToMaybe [ imp | SexpList [SymbolAtom "implicit", BoolAtom imp] <- annotations ] -> return (start, len, Bound True)
    _ -> return (start, len, decor)
decorSpan _ = Nothing

decorStyle :: Decor -> StyleCmd
decorStyle Type = color lightBlue Nothing
decorStyle Data = color red Nothing
decorStyle Function = color lightGreen Nothing
decorStyle (Bound imp) = color pink Nothing ++ if imp then underlined else []

applyDecors :: [(Integer,Integer,Decor)] -> String -> String
applyDecors [] str = str
applyDecors ((start,len,decor):ds) str = let (pre, from) = splitAt (fromInteger start) str
                                             (it, post) = splitAt (fromInteger len) from
                                             update (s,l,d) = (s - (start + len), l, d)
                                         in pre ++ applyStyle (decorStyle decor) it ++ applyDecors (map update ds) post

ellipsis, returnEllipsis :: String
ellipsis = applyStyle (color grey Nothing) "…"
returnEllipsis = applyStyle (color grey Nothing) "↵…"

convertString :: String -> String
convertString str = let ls = filter (not . all isSpace) $ lines str
                        less = if length ls > 5 then take 4 ls ++ [ls !! 4 ++ returnEllipsis] else ls
                        (shortLines, rest) = span ((<= 400) . length) less
                        finalLines = case rest of
                                          [] -> shortLines
                                          (l:_) -> shortLines ++ [take 400 l ++ ellipsis]
                    in unlines finalLines

interpretResp :: SExp -> Maybe (String, Integer)
interpretResp (SexpList [SymbolAtom "return", SexpList [SymbolAtom "ok", StringAtom ""], IntegerAtom _]) = Nothing
interpretResp (SexpList [SymbolAtom "return", SexpList [SymbolAtom "ok", StringAtom res], IntegerAtom i]) = Just (res, i)
interpretResp (SexpList [SymbolAtom "return", SexpList [SymbolAtom "ok", StringAtom res, SexpList ann], IntegerAtom i]) = Just (applyDecors (mapMaybe decorSpan ann) res, i)
interpretResp (SexpList [SymbolAtom "return", SexpList [SymbolAtom "ok", StringAtom res, _], IntegerAtom i]) = Just (res, i)
interpretResp (SexpList [SymbolAtom "return", SexpList [SymbolAtom "error", StringAtom err], IntegerAtom i]) = Just (err, i)
interpretResp (SexpList [SymbolAtom "return", SexpList [SymbolAtom "error", StringAtom err, SexpList ann], IntegerAtom i]) = Just (applyDecors (mapMaybe decorSpan ann) err, i)
interpretResp (SexpList [SymbolAtom "write-string", StringAtom "", IntegerAtom _]) = Nothing
interpretResp (SexpList [SymbolAtom "write-string", StringAtom str, IntegerAtom i]) = Just (str, i)
interpretResp (SexpList [SymbolAtom "set-prompt", StringAtom _, IntegerAtom _]) = Nothing
interpretResp x = error ("what: " ++ show x)

loop :: MIrc -> RetRepo -> Handle -> IO ()
loop mirc r h = do
  sexp <- readResp h
  case interpretResp sexp of
    Nothing -> loop mirc r h
    Just (res, ret) -> do
      (_, m) <- readMVar r
      case Map.lookup ret m of
        Nothing -> do print (res, ret)
                      loop mirc r h
        Just (origin,canceler) -> do
          _ <- tryPutMVar canceler ()
          sendMsg mirc origin . encodeUTF8 . convertString $ res
          loop mirc r h

ircConfig :: ThreadId -> RetRepo -> Handle -> IrcConfig
ircConfig tid r h = (mkDefaultConfig "irc.freenode.net" "idris-ircslave")
  { cUsername = "ircslave"
  , cRealname = "IRC-Idris shim"
  , cChannels = ["#idris","#esoteric"]
  , cEvents = [Privmsg (onMessage tid r h)]
  }

handleExit :: RetRepo -> [Handle] -> ProcessHandle -> MIrc -> SomeException -> IO ()
handleExit rr hs pid mirc ex = do
  print ex
  _ <- tryTakeMVar rr
  disconnect mirc (encodeUTF8 "Terminated")
  mapM_ hClose hs
  terminateProcess pid
  exitSuccess

copyRec :: FilePath -> FilePath -> IO ()
copyRec from to = do
    ex <- doesFileExist from
    if ex then copyFile from to
          else do ex' <- doesDirectoryExist from
                  if ex' then do createDirectory to
                                 allEnts <- getDirectoryContents from
                                 let ents = allEnts \\ [".",".."]
                                 mapM_ (\e -> copyRec (from </> e) (to </> e)) ents
                         else fail $ "copyRec: " ++ show from ++ " does not exist"

mktmpdir :: String -> IO FilePath
mktmpdir name = do
    TOD s _ <- getClockTime
    tmp <- getTemporaryDirectory
    let dirname = tmp </> name <.> show s
    createDirectory dirname
    return dirname

prepareHomedir :: Maybe FilePath -> IO (FilePath, [FilePath], Bool)
prepareHomedir prelude = do
    libdir <- init `fmap` readProcess "idris" ["--libdir"] ""
    homedir <- mktmpdir "idris-ircslave"
    ents <- getDirectoryContents libdir
    let pkgs = ents \\ [".","..","rts","llvm","jsrts","oldeffects"]
    createDirectory $ homedir </> "libs"
    forM_ pkgs $ \pkg -> copyRec (libdir </> pkg) (homedir </> "libs" </> pkg)
    idr <- case prelude of
        Nothing -> return False
        Just f -> do
            copyFile f $ homedir </> "BotPrelude.idr"
            return True
    return (homedir, pkgs, idr)

createIdris :: FilePath -> [FilePath] -> Bool -> CreateProcess
createIdris homedir pkgs idr = (proc "sandbox" $
    [ "-M"
    , "-H", homedir
    , "--"
    , "idris"
    , "--nocolor"
    , "--ideslave"
    ] ++ concatMap (\pkg -> ["-i", "libs" </> pkg]) pkgs ++ ["BotPrelude.idr" | idr])
    { std_in = CreatePipe
    , std_out = CreatePipe
    }

main :: IO ()
main = do
  args <- getArgs
  let (_, botprelude) = case args of
                                 [] -> (Nothing, Nothing)
                                 (x:xs) -> (Just x, listToMaybe xs)
  (homedir, pkgs, idr) <- prepareHomedir botprelude
  (Just toIdris, Just fromIdris, Nothing, idrisPid) <- createProcess $ createIdris homedir pkgs idr
  hSetBuffering toIdris LineBuffering
  hSetBuffering fromIdris LineBuffering
  sendQuery toIdris ":consolewidth 300" 0
  rr <- newMVar (1, Map.empty)
  tid <- myThreadId
  con <- connect (ircConfig tid rr toIdris) True True
  case con of
    Left exc -> print exc
    Right mirc -> loop mirc rr fromIdris `catch` handleExit rr [toIdris, fromIdris] idrisPid mirc


