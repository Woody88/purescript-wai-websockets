module Network.WebSockets.Server where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.ByteString (ByteString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as AVar
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler, runAff_)
import Effect.Aff.AVar as AVarAff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.WebSockets.Connection (Connection(..), PendingConnection(..))
import Network.WebSockets.Connection.Options (ConnectionOptions(..), defaultConnectionOptions)
import Node.HTTP as HTTP
import Node.Net.Socket as Net
import Unsafe.Coerce (unsafeCoerce)
import WebSocket.Server (WSServer, WSServerConfig(..))
import WebSocket.Server as WSS

type ServerApp = PendingConnection -> Effect Unit

runServer ::      
    Int             -- ^ Port to listen on
    -> ServerApp    -- ^ Application
    -> Effect Unit  -- ^ Never returns
runServer port app = do 
    wss        <- WSS.createServer NoServer mempty
    httpserver <- HTTP.createServer (\_ _-> pure unit)
    HTTP.listen httpserver { backlog: Nothing, hostname: "127.0.0.1", port } $ do 
        bus <- AVar.empty
        _ <- makePendingConnection wss httpserver defaultConnectionOptions (const $ pure unit) bus
        launchAff_ $ forever do 
            pc <- AVarAff.take bus 
            liftEffect $ app pc 
            
          

makePendingConnection :: WSServer -> HTTP.Server -> ConnectionOptions -> (Connection -> Effect Unit) -> AVar PendingConnection -> Effect Unit
makePendingConnection wss svr options onAccept bus = 
    onupgrade svr \request socket reqHead -> do 
        let 
            mkPendingConnection = PendingConnection
                { options
                , request 
                , socket 
                , reqHead 
                , wss
                , onAccept: \_ -> Console.log "Connected"
                }
        void $ AVar.put mkPendingConnection bus (\_ -> pure unit)
    
foreign import onupgrade :: HTTP.Server -> (HTTP.Request -> Net.Socket -> ByteString -> Effect Unit) -> Effect Unit 