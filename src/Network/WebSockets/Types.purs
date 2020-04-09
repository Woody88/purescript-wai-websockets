module Network.WebSockets.Types where

import Data.ByteString (ByteString)

data Message
    = ControlMessage ControlMessage
    | DataMessage DataMessage

-- | Different control messages
data ControlMessage
    = Close Int ByteString
    | Ping ByteString
    | Pong ByteString

data DataMessage
    = Text String 
    -- | A binary message.
    | Binary ByteString

data ConnectionType = ServerConnection | ClientConnection