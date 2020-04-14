module Wai.WebSockets where

import Prelude

import Data.ByteString as BS
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Network.Wai (Application, responseSocket) as Wai
import Network.Wai.Internal (Request(..))
import Network.Wai.Internal (Request, Response) as Wai
import Network.WebSockets.Connection (PendingConnection(..)) as WS
import Network.WebSockets.Connection.Options (ConnectionOptions) as WS
import Network.WebSockets.Http (RequestHead(..))
import Network.WebSockets.Http (RequestHead) as WS
import Network.WebSockets.Server (ServerApp) as WS
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
              -> Wai.Request
              -> Maybe Wai.Response
websocketsApp opts app request
    | isWebSocketsReq request = do 
        Just $ Wai.responseSocket \socket ->
            runWebSockets opts request app socket
    | otherwise = Nothing

getRequestHead :: Wai.Request -> WS.RequestHead 
getRequestHead (Request req) = RequestHead
    { path: req.rawPathInfo <> "/" <> req.rawQueryString
    , headers: req.requestHeaders
    , secure: req.isSecure
    }

-- | Returns whether or not the given 'Wai.Request' is a WebSocket request.
isWebSocketsReq :: Wai.Request -> Boolean
isWebSocketsReq (Request req) =
    upgradeRequestHeader == (Just "websocket")
    where 
        requestHeaders = Map.fromFoldable $ req.requestHeaders
        upgradeRequestHeader  = (Map.lookup "upgrade" requestHeaders)

runWebSockets :: forall a.
    WS.ConnectionOptions
    -> Wai.Request
    -> (WS.PendingConnection -> Effect a)
    -> Net.Socket
    -> Effect a
runWebSockets options req@(Request {nodeRequest, rawHeader}) app socket = do 
    let throwBadRequestHandle = throw $ "Bad Wai Request Handle"
    Console.log "hello"
    mrec <- pure do 
        httpreq <- nodeRequest
        rawHead <- rawHeader
        pure {httpreq, rawHead}

    case mrec of 
        Nothing -> throwBadRequestHandle
        Just {httpreq, rawHead} -> do 
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
                    , upgrade: handleUpgrade httpreq (BS.unsafeFreeze rawHead)
                    }
            app pc 
