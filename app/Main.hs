module Main where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
import Control.Concurrent.Chan

import Control.Monad (liftM)
import Control.Monad.Fix (fix)

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  let maxQueuedConnections = 5
  listen sock maxQueuedConnections
  serverChan <- newChan
  mainLoop sock serverChan

mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock serverChan = do
  conn <- accept sock
  forkIO $ runConn conn serverChan
  mainLoop sock serverChan

runConn :: (Socket, SockAddr) -> Chan Msg -> IO ()
runConn (sock, _) serverChan = do
  send sock $ C.pack "Hello.. Please type text to broadcast.. \n"

  clientChan <- dupChan serverChan

  let maxBytesToRecv = 1024

  forkIO $ fix $ \loop -> do
    msg <- recv sock maxBytesToRecv
    writeChan serverChan msg
    loop

  fix $ \loop -> do
    msg <- readChan clientChan
    send sock msg
    loop

  close sock


type Msg = C.ByteString
