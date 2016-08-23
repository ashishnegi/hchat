{-# LANGUAGE MultiParamTypeClasses #-}

module HChat.Network where

-- For Network.Socket library
import qualified Network.Socket as NS (Socket, accept)
import qualified Network.Socket.ByteString as NSB (send, recv)
import qualified Data.ByteString.Char8 as C

-- For WebSockets library
import qualified Network.WebSockets as WS

import Control.Monad (void)

class NetworkConn sock conn where
  send :: conn -> C.ByteString -> IO ()
  recv :: conn -> IO C.ByteString
  accept :: sock -> IO conn

instance NetworkConn NS.Socket NS.Socket where
  send conn text = void $ NSB.send conn text
  recv conn      = NSB.recv conn 1024
  accept sock    = fst <$> NS.accept sock

instance NetworkConn WS.PendingConnection WS.Connection where
  send sock text = WS.sendTextData sock text
  recv sock      = WS.receiveData sock
  accept sock    = WS.acceptRequest sock
