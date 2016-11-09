{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( hsidentdMain
    ) where

import Control.Concurrent
import Control.Concurrent.Async
--import Control.Exception (handle)
import Control.Monad (forever, when, forM, liftM)
import Data.ByteString.Char8 hiding (putStrLn, head, tail, map)
import Network.Socket hiding (recv, send, sendAll)
import Network.Socket.ByteString (recv, send, sendAll)
import qualified Text.Regex.PCRE.Light as Regex
import Text.Printf
import System.Entropy
import Data.Hex
import Data.Char (toLower)
import System.Posix.Process

identRegex = "^\\s*([\\d]{1,5})\\s*,\\s*([\\d]{1,5})\\s*$"
compiledIdentRegex = Regex.compile identRegex []

matchString str = Regex.match re str []
  where re = compiledIdentRegex

getAddressString = show . addrAddress

getRandomString :: IO ByteString
getRandomString = hex <$> getEntropy 4

portMax = 2^16-1
portMin = 1

generateResponse :: Maybe [ByteString] -> ByteString -> ByteString
generateResponse Nothing _ = "0, 0 : ERROR : UNKNOWN-ERROR\r\n"
generateResponse (Just [msg, serverPort, clientPort]) user
  | serverPortVal < portMin || serverPortVal > portMax || clientPortVal < portMin || clientPortVal > portMax = pack $ printf "%s, %s : ERROR : INVALID-PORT\r\n" serverPortStr clientPortStr
  | otherwise = do
      pack $ printf "%s, %s : USERID : UNIX : %s \r\n" serverPortStr clientPortStr userStr
  where
    userStr = map toLower $ unpack user
    serverPortStr = unpack serverPort
    clientPortStr = unpack clientPort
    serverPortVal = read serverPortStr
    clientPortVal = read clientPortStr

handleClient :: Socket -> IO ()
handleClient client = do
  msg <- doRecv
  user <- getRandomString
  send client $ generateResponse (matchString msg) user
  close client
  where
    bufSize = 1024
    doRecv = recv client bufSize

acceptClient :: Socket -> IO ()
acceptClient socket = forever $ do
  (client, _) <- accept socket
  threadId <- forkIO $ handleClient client
  return ()

setSockOpt :: Socket -> SocketOption -> Int -> IO ()
setSockOpt sock opt val
  | isSupportedSocketOption opt = setSocketOption sock opt val
  | otherwise = return ()
  
listener :: Family -> IO ()
listener af = do
  serverAddr <- head <$> myGetAddrInfo
  sock <- socket af Stream defaultProtocol
  when (af == AF_INET6) $ setSockOpt sock IPv6Only 1
  setSockOpt sock ReuseAddr 1
  setSockOpt sock ReusePort 1
  bindSocket sock (addrAddress serverAddr)
  listen sock backlog
  acceptClient sock
  where hints = defaultHints { addrFamily = af
                             , addrSocketType = Stream
                             }
        backlog = 5
        listenHost = "localhost"
        port = "1130"
        myGetAddrInfo = getAddrInfo (Just hints) (Just listenHost) (Just port)

mainLoop :: IO ()
mainLoop = withSocketsDo $ do
  listeners <- mapM asyncListener addressFamilies
  forever $ waitAnyCatchCancel listeners
  where
    asyncListener = async . listener
    addressFamilies = [AF_INET, AF_INET6] :: [Family]

daemonize :: IO () -> IO ()
daemonize f = do
  _ <- createSession
  _ <- forkProcess f
  return ()
  
hsidentdMain :: IO ()
hsidentdMain = do
  _ <- forkProcess f
  return ()
  where
    f = do
      _ <- daemonize mainLoop
      return ()

