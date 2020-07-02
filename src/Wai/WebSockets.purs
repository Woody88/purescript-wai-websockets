module Wai.WebSockets where

import Prelude

import Data.ByteString as BS
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Network.Wai (Response, headers, isSecure, responseSocket, url) as Wai
import Network.Wai.Http (Application, HttpRequest) as Wai
import Network.WebSockets.Connection (PendingConnection(..)) as WS
import Network.WebSockets.Connection.Options (ConnectionOptions) as WS
import Network.WebSockets.Http (RequestHead(..))
import Network.WebSockets.Http (RequestHead) as WS
import Network.WebSockets.Server (ServerApp) as WS
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Net.Socket as Net
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
              -> Wai.HttpRequest
              -> Maybe Wai.Response
websocketsApp opts app request
    | isWebSocketsReq request = do 
        Just $ Wai.responseSocket \socket rawH -> liftEffect do 
            rawHeader <- maybe (Buffer.create 0) pure rawH
            runWebSockets opts request app socket rawHeader

    | otherwise = Nothing

getRequestHead :: Wai.HttpRequest -> WS.RequestHead 
getRequestHead req = RequestHead
    { path: Wai.url req 
    , headers: Wai.headers req
    , secure: Wai.isSecure req
    }

-- | Returns whether or not the given 'Wai.Request' is a WebSocket request.
isWebSocketsReq :: Wai.HttpRequest -> Boolean
isWebSocketsReq req =
    upgradeRequestHeader == (Just "websocket")
    where 
        requestHeaders = Map.fromFoldable $ Wai.headers req 
        upgradeRequestHeader  = (Map.lookup (wrap "upgrade") requestHeaders)

runWebSockets :: forall a.
    WS.ConnectionOptions
    -> Wai.HttpRequest
    -> (WS.PendingConnection -> Effect a)
    -> Net.Socket
    -> Buffer
    -> Effect a
runWebSockets options req app socket rawH = do 
    let httpreq = unwrap req
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
