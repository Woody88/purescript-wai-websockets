module Network.WebSockets.Connection where

import Prelude

import Data.ByteString (ByteString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_, makeAff, nonCanceler, try)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Network.HTTP.Types (RequestHeaders)
import Network.WebSockets.Connection.Options (ConnectionOptions)
import Network.WebSockets.Types (ConnectionType(..), Message)
import Node.HTTP as HTTP
import Node.Net.Socket as Net
import Unsafe.Coerce (unsafeCoerce)
import WebSocket (WebSocket)
import WebSocket as WS
import WebSocket.Server (WSServer)
import WebSocket.Server as WSS

newtype Connection = Connection
    { options    :: ConnectionOptions
    , type       :: ConnectionType
    , parse      :: Effect (Maybe Message)
    , write      :: Array Message -> Effect Unit
    , websocket  :: WebSocket
    , sentClose  :: Ref Boolean 
    }

newtype PendingConnection = PendingConnection
    { options  :: ConnectionOptions
    -- ^ Options, passed as-is to the 'Connection'
    , reqHead  :: ByteString 
    , request  :: HTTP.Request
    , socket   :: Net.Socket
    -- ^ Useful for e.g. inspecting the request path.
    , onAccept :: Connection -> Effect Unit
    -- ^ One-shot callback fired when a connection is accepted, i.e., *after*
    -- the accepting response is sent to the client.
    , wss             :: WSServer
    }

newtype AcceptRequest = AcceptRequest
    { acceptSubprotocol :: Maybe ByteString
    -- ^ The subprotocol to speak with the client.  If 'pendingSubprotcols' is
    -- non-empty, 'acceptSubprotocol' must be one of the subprotocols from the
    -- list.
    , acceptHeaders :: RequestHeaders
    -- ^ Extra headers to send with the response.
    }

defaultAcceptRequest :: AcceptRequest
defaultAcceptRequest = AcceptRequest 
    { acceptSubprotocol: Nothing 
    , acceptHeaders: mempty 
    }

acceptRequest :: PendingConnection -> AcceptRequest -> Aff Connection 
acceptRequest (PendingConnection {wss, onAccept, options, request, socket, reqHead}) _ = makeAff \done -> do 
    Console.log "in"
    WSS.handleUpgrade wss request socket reqHead \ws -> do 
        conn <- mkConnection ws
        onAccept conn 
        done $ pure conn 
    pure nonCanceler

    where 
        mkConnection ws = do 
            ref <- Ref.new true
            pure $ Connection
                { options: options
                , type: ServerConnection
                , parse: pure Nothing 
                , write: \_ -> pure unit 
                , websocket: ws 
                , sentClose: ref
                }

withPingThread :: forall a. 
    Connection
    -> Number  -- ^ Second interval in which pings should be sent.
    -> Effect Unit  -- ^ Repeat this after sending a ping.
    -> Aff a   -- ^ Application to wrap with a ping thread.
    -> Aff a   -- ^ Executes application and kills ping thread when done.
withPingThread c@(Connection conn) n action app = do 
    liftEffect $ WS.onpong conn.websocket \_ -> do
        Console.log "got pong"
        Ref.modify_ (const true) conn.sentClose
    Aff.finally (pingThread c n action) app 

pingThread :: Connection -> Number -> Effect Unit -> Aff Unit 
pingThread _ 0.00 _ = pure unit 
pingThread c@(Connection conn) n action = do 
    delay $ Milliseconds n
    alive <- liftEffect $ Ref.read conn.sentClose
    Console.log $ show alive
    liftEffect $ when (not alive) (Console.log "broken" *> WS.terminate conn.websocket)
    ping alive 
    where 
        ping :: Boolean -> Aff Unit 
        ping false = pure unit 
        ping true  = do 
            liftEffect do 
                Ref.modify_ (const false) conn.sentClose 
                WS.ping conn.websocket action 
            pingThread c n action
