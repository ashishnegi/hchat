module Main where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import Control.Concurrent

import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Exception

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  let maxQueuedConnections = 5
  listen sock maxQueuedConnections
  serverChan <- newChan

  forkIO $ fix $ \loop -> do
    (_, a) <- readChan serverChan -- needed to prevent memory leaks
    loop

  mainLoop sock serverChan 0

mainLoop :: Socket -> Chan Msg -> ClientId -> IO ()
mainLoop sock serverChan clientId = do
  conn <- accept sock
  forkIO $ runConn conn serverChan clientId
  mainLoop sock serverChan $ clientId + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> ClientId -> IO ()
runConn (sock, _) serverChan id = do
  send sock $ C.pack "Hello.. Please type text (< 1024 chars) to broadcast.. \n"

  clientChan <- dupChan serverChan

  reader <- forkIO $ fix $ \loop -> do
    (senderId, msg) <- readChan clientChan
    when (senderId /= id) $ send sock msg >> return ()
    loop

  let maxBytesToRecv = 1024
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    msg <- recv sock maxBytesToRecv
    case cleanByteString msg of
      "quit" -> send sock (C.pack "Sayonara...!!!") >> return ()
      _ -> writeChan serverChan (id, msg) >> loop

  killThread reader
  writeChan serverChan (id, C.pack "<------- User left --------->\n")
  close sock


type ClientId = Int
type Msg = (ClientId, C.ByteString)

cleanByteString = reverse . dropWhile (\c -> c == '\n' || c == '\r') . reverse . C.unpack
