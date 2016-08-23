module HChat.Network where

-- For Network.Socket library
import qualified Network.Socket as NS (Socket, accept)
import qualified Network.Socket.ByteString as NSB (send, recv)
import qualified Data.ByteString.Char8 as C

-- For WebSockets library
import qualified Network.WebSockets as WS

import Control.Monad (void)

class NetworkConn sock where
  send :: sock -> C.ByteString -> IO ()
  recv :: sock -> IO (C.ByteString)
  accept :: (NetworkConn myconn) => sock -> IO myconn

instance NetworkConn NS.Socket where
  send sock text = void $ NSB.send sock text
  recv sock      = NSB.recv sock 1024
  accept sock    = fst <$> NS.accept sock

instance NetworkConn WS.Connection where
  send sock text = WS.sendTextData sock text
  recv sock      = WS.receiveData sock
  accept sock    = error "Not allowed"

instance NetworkConn WS.PendingConnection where
  send sock text = error "Not allowed"
  recv sock      = error "Not allowed"
  accept sock    = WS.acceptRequest sock
