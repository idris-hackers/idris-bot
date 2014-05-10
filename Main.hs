{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Main where

import IdeSlave (SExp(..), parseSExp, convSExp)
import IrcColor

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, myThreadId, throwTo, ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, takeMVar, tryTakeMVar, putMVar, readMVar, tryPutMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (replicateM, forM_, msum)
import Control.Monad.Error (MonadError)
import Control.Monad.State (execStateT, put, lift)
import qualified Control.Monad.State as State (get)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.ConfigFile (ConfigParser(optionxform), CPError, emptyCP, set, setshow, readfile, get, sections)
import Data.Either.Utils (forceEither)
import Data.List (stripPrefix, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Network.SimpleIRC
import Numeric (readHex)
import System.Directory (createDirectory, getTemporaryDirectory, doesFileExist, doesDirectoryExist, copyFile, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>), (<.>))
import System.IO (Handle, hGetChar, hPutStrLn, hSetBuffering, BufferMode(..), hClose)
import System.Process (proc, CreateProcess(..), StdStream(..), createProcess, readProcess, ProcessHandle, terminateProcess)
import System.Time (ClockTime(..), getClockTime)
import System.Timeout (timeout)

decodeUTF8 :: ByteString -> String
decodeUTF8 = unpack . decodeUtf8With lenientDecode

encodeUTF8 :: String -> ByteString
encodeUTF8 = encodeUtf8 . pack

type RetRepo = MVar (Integer, Map Integer (ByteString, String, MVar ()))

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

commandAllowed :: [String] -> String -> Maybe String
commandAllowed allowed q = case dropWhile isSpace q of
  ':':cs -> case words cs of
              [] -> Nothing
              (w:_) | w `elem` allowed -> Nothing
                    | otherwise -> Just w
  _ -> Nothing

onMessage :: ConfigParser -> ThreadId -> RetRepo -> Handle -> EventFunc
onMessage config mainth rets h mirc msg =
  do nick <- decodeUTF8 <$> getNickname mirc
     let sentTo = maybe "noChannel" decodeUTF8 $ mChan msg
         confsection = if sentTo == nick then "noChannel" else sentTo
         interpPrefixes = forceEither $ get config confsection "interpPrefixes"
         allowedCommands = forceEither $ get config confsection "allowedCommands"
         msgText = decodeUTF8 $ mMsg msg
         msgQuery = fromMaybe msgText $ stripPrefix (nick ++ ": ") msgText
     case mOrigin msg of
       Nothing -> return ()
       Just origin -> case msum . map (`stripPrefix` msgQuery) $ interpPrefixes of
         Nothing -> return ()
         Just msgCommand -> case commandAllowed allowedCommands msgCommand of
           Just cmd -> sendMsg mirc origin . encodeUTF8 $ "Command " ++ show cmd ++ " not permitted."
           Nothing -> do
             (qid, qmap) <- takeMVar rets
             sendQuery h msgCommand qid
             canceler <- setTimeout (10 * 10 ^ (6 :: Int)) mainth
             putMVar rets (qid + 1, Map.insert qid (origin, confsection, canceler) qmap)
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

data Decor = Keyword | Type | Data | Function | Bound Bool -- is it implicit?
  deriving (Show, Read, Eq)

readDecor :: String -> Maybe Decor
readDecor "keyword" = Just Keyword
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
decorStyle Keyword = bold
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

limitString :: Maybe Int -> Maybe Int -> String -> String
limitString maxChars maxLines str =
  let nonemptyLines = filter (not . all isSpace) $ lines str
      fewerLines = case maxLines of
                     Nothing -> nonemptyLines
                     Just len -> case splitAt (len-1) nonemptyLines of
                       (initLines, lastLine : _ : _) -> initLines ++ [lastLine ++ returnEllipsis]
                       _ -> nonemptyLines
      shorterLines = case maxChars of
                       Nothing -> fewerLines
                       Just len -> case span ((<= len) . length) fewerLines of
                         (shortLines, longLine : _) -> shortLines ++ [take (len - length ellipsis) longLine ++ ellipsis]
                         _ -> fewerLines
  in unlines shorterLines

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

loop :: ConfigParser -> MIrc -> RetRepo -> Handle -> IO ()
loop config mirc rets h = let continue = loop config mirc rets h in do
  sexp <- readResp h
  case interpretResp sexp of
    Nothing -> continue
    Just (res, ret) -> do
      (_, m) <- readMVar rets
      case Map.lookup ret m of
        Nothing -> do print (res, ret)
                      continue
        Just (origin, confsection, canceler) -> do
          let maxCharsPerLine = forceEither $ get config confsection "maxCharsPerLine"
              maxLinesPerResponse = forceEither $ get config confsection "maxLinesPerResponse"
          _ <- tryPutMVar canceler ()
          sendMsg mirc origin . encodeUTF8 . limitString maxCharsPerLine maxLinesPerResponse $ res
          continue

defaultConfig :: ConfigParser
defaultConfig = forceEither . flip execStateT emptyCP {optionxform = id} $ do
  set' "nick" "idris-ircslave"
  sets' "channels" ([] :: [String])
  sets' "maxCharsPerLine" $ Just (400 :: Int)
  sets' "maxLinesPerResponse" $ Just (5 :: Int)
  sets' "consoleWidth" $ Just (200 :: Int)
  sets' "allowedCommands" ["t", "type"]
  sets' "interpPrefixes" ["> "]
    where modifyM f = State.get >>= lift . f >>= put
          set' k v = modifyM $ \conf -> set conf "DEFAULT" k v
          sets' k v = modifyM $ \conf -> setshow conf "DEFAULT" k v

ircConfig :: ConfigParser -> ThreadId -> RetRepo -> Handle -> IrcConfig
ircConfig config tid r h = forceEither $ do
  network <- get config "DEFAULT" "network"
  nick <- get config "DEFAULT" "nick"
  channels <- get config "DEFAULT" "channels"
  return $ (mkDefaultConfig network nick)
    { cUsername = "ircslave"
    , cRealname = "IRC-Idris shim"
    , cChannels = channels
    , cEvents = [Privmsg (onMessage config tid r h)]
    }

checkConfig :: MonadError CPError m => ConfigParser -> m ()
checkConfig config = do
  _ :: String <- get config "DEFAULT" "network"
  _ :: String <- get config "DEFAULT" "nick"
  _ :: [String] <- get config "DEFAULT" "channels"
  _ :: Maybe Int <- get config "DEFAULT" "consoleWidth"
  forM_ ("DEFAULT" : sections config) $ \sec -> do
    _ :: Maybe Int <- get config sec "maxCharsPerLine"
    _ :: Maybe Int <- get config sec "maxLinesPerResponse"
    _ :: [String] <- get config sec "allowedCommands"
    _ :: [String] <- get config sec "interpPrefixes"
    return ()

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
  (configfile, botprelude) <- case args of
                                [] -> do putStrLn "A configuration file is required, see documentation"
                                         exitFailure
                                (x:xs) -> return (x, listToMaybe xs)
  config <- forceEither <$> readfile defaultConfig configfile
  case checkConfig config of
    Left err -> do print err
                   exitFailure
    Right () -> return ()
  (homedir, pkgs, idr) <- prepareHomedir botprelude
  (Just toIdris, Just fromIdris, Nothing, idrisPid) <- createProcess $ createIdris homedir pkgs idr
  hSetBuffering toIdris LineBuffering
  hSetBuffering fromIdris LineBuffering
  case forceEither $ get config "DEFAULT" "consoleWidth" of
    Nothing -> return ()
    Just width -> sendQuery toIdris (":consolewidth " ++ show (width :: Int)) 0
  rr <- newMVar (1, Map.empty)
  tid <- myThreadId
  con <- connect (ircConfig config tid rr toIdris) True True
  case con of
    Left exc -> print exc
    Right mirc -> loop config mirc rr fromIdris `catch` handleExit rr [toIdris, fromIdris] idrisPid mirc


