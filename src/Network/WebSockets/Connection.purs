module Network.WebSockets.Connection where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay, error, forkAff, makeAff, nonCanceler, runAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Network.HTTP.Types (RequestHeaders)
import Network.WebSockets.Connection.Options (ConnectionOptions)
import Network.WebSockets.Types (class WebSocketsData, ConnectionType(..), ControlMessage(..), DataMessage(..), Message(..), fromByteString, fromDataMessage, toByteString)
import Node.HTTP as HTTP
import Node.Net.Socket as Net
import WebSocket (WebSocket)
import WebSocket as WS
import WebSocket.Server (WSServer)
import WebSocket.Server as WSS

newtype Connection = Connection
    { options    :: ConnectionOptions
    , type       :: ConnectionType
    , parse      :: Aff (Maybe Message)
    , write      :: Array Message -> Aff Unit
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

receive :: Connection -> Aff Message
receive c@(Connection conn) = do
    mbMsg <- conn.parse
    case mbMsg of
        Nothing  -> throwError $ error "ConnectionClosed"
        Just msg -> pure msg

receiveDataMessage :: Connection -> Aff DataMessage
receiveDataMessage c@(Connection conn) = do
    msg <- receive c
    case msg of
        DataMessage dm    -> pure dm
        ControlMessage cm    
            | Close _ _ <- cm -> do
                liftEffect do 
                    hasSentClose <- Ref.read conn.sentClose 
                    WS.terminate conn.websocket 
                Console.log "out"
                throwError $ error "CloseRequest"
            | otherwise   -> throwError $ error "not implemented"

receiveData :: forall a. WebSocketsData a => Connection -> Aff a
receiveData conn = fromDataMessage <$> receiveDataMessage conn

send :: Connection -> Message -> Aff Unit 
send conn = sendAll conn <<< pure 

sendAll :: Connection -> Array Message -> Aff Unit
sendAll _    []   = pure unit
sendAll c@(Connection conn) msgs = do
    liftEffect $ when (any isCloseMessage msgs) $
        Ref.write true conn.sentClose
    conn.write msgs
  where
    isCloseMessage (ControlMessage (Close _ _)) = true
    isCloseMessage _                            = false

sendDataMessage :: Connection -> DataMessage -> Aff Unit
sendDataMessage conn = sendDataMessages conn <<< pure 

sendDataMessages :: Connection -> Array DataMessage -> Aff Unit
sendDataMessages conn = sendAll conn <<< map DataMessage 

sendTextData :: forall a. WebSocketsData a => Connection -> a -> Aff Unit
sendTextData conn = sendTextDatas conn <<< pure

sendTextDatas :: forall a. WebSocketsData a => Connection -> Array a -> Aff Unit 
sendTextDatas conn = 
    sendDataMessages conn <<< map (Text <<< toByteString)

sendBinaryData :: forall a. WebSocketsData a => Connection -> a -> Aff Unit 
sendBinaryData conn = sendBinaryDatas conn <<< pure 

sendBinaryDatas :: forall a. WebSocketsData a => Connection -> Array a -> Aff Unit
sendBinaryDatas conn = sendDataMessages conn <<< map (Binary <<< toByteString)

sendClose :: forall a. WebSocketsData a => Connection -> a -> Aff Unit
sendClose conn = sendCloseCode conn 1000

sendCloseCode :: forall a. WebSocketsData a => Connection -> Int -> a -> Aff Unit
sendCloseCode conn code =
    send conn <<< ControlMessage <<< Close code <<< toByteString

acceptRequest :: PendingConnection -> AcceptRequest -> Aff Connection 
acceptRequest (PendingConnection {wss, onAccept, options, request, socket, reqHead}) _ = makeAff \done -> do 
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
                , parse: parseMessage ws
                , write: \_ -> pure unit 
                , websocket: ws 
                , sentClose: ref
                }

parseMessage :: WebSocket -> Aff (Maybe Message) 
parseMessage ws = makeAff \done -> do 
    WS.onmessage ws (done <<< pure <<< Just <<< DataMessage <<< Binary)
    pure nonCanceler
    
withPingThread :: forall a. 
    Connection
    -> Number       -- ^ Second interval in which pings should be sent.
    -> Effect Unit  -- ^ Repeat this after sending a ping.
    -> Aff a        -- ^ Application to wrap with a ping thread.
    -> Aff a       -- ^ Executes application and kills ping thread when done.
withPingThread c@(Connection conn) n action app = do 
    liftEffect $ WS.onpong conn.websocket \_ -> do
        Ref.modify_ (const true) conn.sentClose
    _ <- forkAff do pingThread c n action
    app   

pingThread :: Connection -> Number -> Effect Unit -> Aff Unit 
pingThread _ 0.00 _ = pure unit 
pingThread c@(Connection conn) n action = do 
    delay $ Milliseconds n
    alive <- liftEffect $ Ref.read conn.sentClose
    liftEffect $ when (not alive) (WS.terminate conn.websocket)
    ping alive 
    where 
        ping :: Boolean -> Aff Unit 
        ping false = pure unit 
        ping true  = do 
            liftEffect do 
                Ref.modify_ (const false) conn.sentClose 
                WS.ping conn.websocket action 
            pingThread c n action
