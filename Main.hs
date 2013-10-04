module Main where

import IdeSlave (SExp(..), parseSExp, convSExp)

import Prelude hiding (catch)

import Control.Concurrent (forkIO, myThreadId, throwTo, ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, takeMVar, tryTakeMVar, putMVar, readMVar, tryPutMVar)
import Control.Exception (SomeException, catch, toException)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.List (find, isPrefixOf, stripPrefix, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Network.SimpleIRC
import Numeric (readHex)
import System.Exit (exitSuccess)
import System.IO (Handle, hGetChar, hPutStrLn, hSetBuffering, BufferMode(..), hClose)
import System.Posix.Signals (Handler(..), installHandler, keyboardSignal, softwareTermination)
import System.Process (proc, CreateProcess(..), StdStream(..), createProcess, waitForProcess, ProcessHandle, terminateProcess)
import System.Timeout


decodeUTF8 = unpack . decodeUtf8With lenientDecode
encodeUTF8 = encodeUtf8 . pack

type RetRepo = MVar (Integer, Map Integer (ByteString, MVar ()))

setTimeout :: Int -> ThreadId-> IO (MVar ())
setTimeout n tid = do
  canceler <- newEmptyMVar
  forkIO $ do
    m <- timeout n $ takeMVar canceler
    case m of
      Just _ -> return ()
      Nothing -> throwTo tid $ userError "timeout"
  return canceler

sendQuery :: Handle -> String -> Integer -> IO ()
sendQuery h q i = hPutStrLn h $ convSExp "interpret" q i

filterQuery :: String -> Maybe String
filterQuery q = case dropWhile isSpace q of
  ':':'t':s:_ | isSpace s -> Nothing
  ':':'t':'y':'p':'e':s:_ | isSpace s -> Nothing
  ':':'i':s:_ | isSpace s -> Nothing
  ':':'i':'n':'f':'o':s:_ | isSpace s -> Nothing
  ':':'d':'o':'c':s:_ | isSpace s -> Nothing
  ':':_ -> Just "Command not permitted"
  _ -> Nothing

onMessage :: ThreadId -> RetRepo -> Handle -> (MIrc -> SomeException -> IO ()) -> EventFunc
onMessage mainth r h hExit mirc msg = case stripPrefix "> " (decodeUTF8 (mMsg msg)) of
                                 Nothing -> return ()
                                 Just query -> case mOrigin msg of
                                   Nothing -> return ()
                                   Just origin -> case filterQuery query of
                                     Just err -> sendMsg mirc origin . encodeUTF8 $ err
                                     Nothing -> do
                                       (i, m) <- takeMVar r
                                       sendQuery h query i
                                       canceler <- setTimeout (10 * 10^6) mainth
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

convertString :: String -> String
convertString = intercalate "  " . filter (not . null) . map (dropWhile isSpace) . lines

interpretResp :: SExp -> Maybe (String, Integer)
interpretResp (SexpList [SymbolAtom "return", SexpList [SymbolAtom "ok", StringAtom ""], IntegerAtom _]) = Nothing
interpretResp (SexpList [SymbolAtom "return", SexpList [SymbolAtom "ok", StringAtom res], IntegerAtom i]) = Just (res, i)
interpretResp (SexpList [SymbolAtom "return", SexpList [SymbolAtom "error", StringAtom err], IntegerAtom i]) = Just ("error: " ++ err, i)
interpretResp (SexpList [SymbolAtom "write-string", StringAtom "", IntegerAtom _]) = Nothing
interpretResp (SexpList [SymbolAtom "write-string", StringAtom str, IntegerAtom i]) = Just (str, i)
interpretResp x = error ("what: " ++ show x)

truncateStr :: Int -> String -> String
truncateStr n xs | length xs <= n = xs
                 | otherwise = take n xs ++ "…"

loop :: MIrc -> RetRepo -> Handle -> IO ()
loop mirc r h = do
  sexp <- readResp h
  case interpretResp sexp of
    Nothing -> loop mirc r h
    Just (res, ret) -> do
      (i, m) <- readMVar r
      case Map.lookup ret m of
        Nothing -> loop mirc r h
        Just (origin,canceler) -> do
          tryPutMVar canceler ()
          sendMsg mirc origin . encodeUTF8 . truncateStr 300 . convertString $ res
          loop mirc r h

config tid r h hExit = defaultConfig
  { cAddr = "irc.freenode.net"
  , cNick = "idris-ircslave"
  , cUsername = "ircslave"
  , cRealname = "IRC-Idris shim"
  , cChannels = ["#idris"]
  , cEvents = [Privmsg (onMessage tid r h hExit)]
  }

handleExit :: RetRepo -> [Handle] -> ProcessHandle -> MIrc -> SomeException -> IO ()
handleExit rr hs pid mirc ex = do
  print ex
  tryTakeMVar rr
  disconnect mirc (encodeUTF8 "Terminated")
  mapM_ hClose hs
  terminateProcess pid
  exitSuccess

createIdris = (proc "sandbox"
    [ "-M"
    , "-i", "/home/melvar/.cabal/share/x86_64-linux-ghc-7.4.2/idris-0.9.9.1/"
    , "idris"
    , "-i", ".cabal/share/x86_64-linux-ghc-7.4.2/idris-0.9.9.1/base"
    , "--nocolor"
    , "--ideslave"
    ])
    { std_in = CreatePipe
    , std_out = CreatePipe
    }

main :: IO ()
main = do
  (Just toIdris, Just fromIdris, Nothing, idrisPid) <- createProcess createIdris
  hSetBuffering toIdris LineBuffering
  hSetBuffering fromIdris LineBuffering
  rr <- newMVar (0, Map.empty)
  tid <- myThreadId
  con <- connect (config tid rr toIdris $ handleExit rr [toIdris, fromIdris] idrisPid) True True
  case con of
    Left exc -> print exc
    Right mirc -> do
--      installHandler keyboardSignal (Catch $ handleExit mirc rr [toIdris, fromIdris, errIdris] idrisPid) Nothing
--      installHandler softwareTermination (Catch $ handleExit mirc rr [toIdris, fromIdris, errIdris] idrisPid) Nothing
      loop mirc rr fromIdris `catch` handleExit rr [toIdris, fromIdris] idrisPid mirc


