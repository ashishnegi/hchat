module HChat.Network where

-- For Network.Socket library
import qualified Network.Socket as NS (Socket, accept)
import qualified Network.Socket.ByteString as NSB (send, recv)
import qualified Data.ByteString.Char8 as C

-- For WebSockets library
import qualified Network.WebSockets as WS

import Control.Monad (void)

class NetworkSocket sock where
  send :: sock -> C.ByteString -> IO ()
  recv :: sock -> IO (C.ByteString)
  accept :: sock -> IO sock

instance NetworkSocket NS.Socket where
  send sock text = void $ NSB.send sock text
  recv sock      = NSB.recv sock 1024
  accept sock    = fst <$> NS.accept sock


instance NetworkSocket WS.PendingConnection where
  send sock text = error "Not allowed"
  recv sock      = error "Not allowed"
  accept sock    = error "Not implemented"
