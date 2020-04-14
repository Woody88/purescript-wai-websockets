module Network.WebSockets.Server where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.ByteString (ByteString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as AVar
import Effect.Aff (launchAff_, makeAff, nonCanceler)
import Effect.Aff.AVar as AVarAff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.WebSockets.Connection (Connection, PendingConnection(..))
import Network.WebSockets.Connection.Options (ConnectionOptions, defaultConnectionOptions)
import Network.WebSockets.Http as Http
import Node.HTTP as HTTP
import Node.Net.Socket as Net
import WebSocket.Server (WSServerConfig(..))
import WebSocket.Server as WSS

type ServerApp = PendingConnection -> Effect Unit

runServer ::      
    Int             -- ^ Port to listen on
    -> ServerApp    -- ^ Application
    -> Effect Unit  -- ^ Never returns
runServer port app = do 
    httpserver <- HTTP.createServer (\_ _-> pure unit)
    HTTP.listen httpserver { backlog: Nothing, hostname: "127.0.0.1", port } $ do 
        bus <- makePendingConnection httpserver defaultConnectionOptions (const $ pure unit)
        launchAff_ $ forever do 
            pc <- AVarAff.take bus 
            liftEffect $ app pc 

makePendingConnection :: HTTP.Server -> ConnectionOptions -> (Connection -> Effect Unit) -> Effect (AVar PendingConnection)
makePendingConnection svr options onAccept = do
    bus <- AVar.empty
    wss <- WSS.createServer NoServer mempty

    let handleUpgrade req reqH sck = makeAff \done -> do 
            WSS.handleUpgrade wss req sck reqH \ws -> do 
                done $ pure ws 
            pure nonCanceler

    onupgrade svr \req socket reqHead -> do 
        let 
            mkPendingConnection = PendingConnection
                { options
                , socket 
                , request: Http.mkRequestHead req false
                , onAccept: \_ -> Console.log "Connected"
                , upgrade: handleUpgrade req reqHead
                }
        void $ AVar.put mkPendingConnection bus (\_ -> pure unit)
    pure bus

foreign import onupgrade :: HTTP.Server -> (HTTP.Request -> Net.Socket -> ByteString -> Effect Unit) -> Effect Unit