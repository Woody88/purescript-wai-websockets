module Network.WebSockets.Types where

import Prelude

import Data.ByteString (ByteString)
import Data.ByteString as BS

data Message
    = ControlMessage ControlMessage
    | DataMessage DataMessage

-- | Different control messages
data ControlMessage
    = Close Int ByteString
    | Ping ByteString
    | Pong ByteString

data DataMessage
    = Text ByteString 
    -- | A binary message.
    | Binary ByteString

data ConnectionType = ServerConnection | ClientConnection

class WebSocketsData a where
    fromDataMessage :: DataMessage -> a

    fromByteString :: ByteString -> a
    toByteString   :: a -> ByteString


instance webSocketsDataBS :: WebSocketsData ByteString where 
    fromDataMessage (Text t)   = toByteString t
    fromDataMessage (Binary b) = b  
    fromByteString = identity
    toByteString   = identity

instance webSocketsDataStr :: WebSocketsData String where 
    fromDataMessage (Text t)   = fromByteString t
    fromDataMessage (Binary b) = fromByteString b 
    fromByteString = BS.fromUTF8
    toByteString   = BS.toUTF8