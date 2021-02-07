module Network.WebSockets.Client where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, runAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Network.WebSockets.Connection (Connection(..), encodeMessage, parseMessage)
import Network.WebSockets.Connection.Options (defaultConnectionOptions)
import Network.WebSockets.Types (ConnectionType(..))
import WebSocket as WS

type ClientApp a = Connection -> Aff a

runClient :: forall a. 
    String          -- ^ Address
    -> ClientApp a  -- ^ Client application
    -> Aff a
runClient addr capp = do 
    ws <- liftEffect $ WS.createWebsocket addr [] mempty
    
    makeAff \cb -> do
        WS.onopen ws do 
            conn <- mkConnection ws 
            runAff_ cb (capp conn)
        pure nonCanceler
        
    where
        mkConnection ws = do 
            sentClose <- Ref.new true
            pure $ Connection
                { options: defaultConnectionOptions
                , type: ClientConnection
                , parse: parseMessage ws
                , write: encodeMessage ws
                , websocket: ws
                , sentClose
                }

