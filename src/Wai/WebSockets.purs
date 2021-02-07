module Wai.WebSockets where

import Prelude

import Data.ByteString as BS
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Network.Wai (Application, Request, Response, responseSocket) as Wai
import Network.WebSockets.Connection (PendingConnection(..)) as WS
import Network.WebSockets.Connection.Options (ConnectionOptions) as WS
import Network.WebSockets.Http (RequestHead(..))
import Network.WebSockets.Http (RequestHead) as WS
import Network.WebSockets.Server (ServerApp) as WS
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Net.Socket as Net
import Wai.Websockets.Internal (withNodeRequest)
import WebSocket.Server (WSServerConfig(..))
import WebSocket.Server as WSS

websocketsOr :: WS.ConnectionOptions
             -> WS.ServerApp
             -> Wai.Application
             -> Wai.Application
websocketsOr opts app backup req sendResponse =
    case websocketsApp opts app req of
        Nothing -> backup req sendResponse
        Just res -> sendResponse res

websocketsApp :: WS.ConnectionOptions
              -> WS.ServerApp
              -> Wai.Request
              -> Maybe Wai.Response
websocketsApp opts app request
    | isWebSocketsReq request = do 
        Just $ Wai.responseSocket \socket rawH -> liftEffect do 
            rawHeader <- maybe (Buffer.create 0) pure rawH
            runWebSockets opts request app socket rawHeader

    | otherwise = Nothing

getRequestHead :: Wai.Request -> WS.RequestHead 
getRequestHead req = RequestHead
    { path: _.url $ unwrap req 
    , headers: _.headers $ unwrap req
    , secure: _.isSecure $ unwrap req
    }

-- | Returns whether or not the given 'Wai.Request' is a WebSocket request.
isWebSocketsReq :: Wai.Request -> Boolean
isWebSocketsReq req =
    upgradeRequestHeader == (Just "websocket")
    where 
        requestHeaders = Map.fromFoldable $ _.headers $ unwrap req 
        upgradeRequestHeader  = (Map.lookup (wrap "upgrade") requestHeaders)

runWebSockets ::
    WS.ConnectionOptions
    -> Wai.Request
    -> (WS.PendingConnection -> Effect Unit)
    -> Net.Socket
    -> Buffer
    -> Effect Unit
runWebSockets options req app socket rawH = withNodeRequest req \httpreq -> do 
    wss <- WSS.createServer NoServer mempty
    let handleUpgrade req' reqH sck = makeAff \done -> do 
            WSS.handleUpgrade wss req' sck reqH \ws -> do 
                done $ pure ws 
            pure nonCanceler

        pc = WS.PendingConnection
            { options 
            , request: getRequestHead req
            , onAccept: \_ -> pure unit 
            , socket
            , upgrade: handleUpgrade httpreq (BS.unsafeFreeze rawH)
            }
    app pc 
