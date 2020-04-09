module Examples.Server where

import Prelude

import Control.Monad.Rec.Class (forever)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Network.WebSockets.Connection (acceptRequest, defaultAcceptRequest, receiveData, withPingThread) as WS
import Network.WebSockets.Connection as Ws
import Network.WebSockets.Server (ServerApp, runServer) as WS

main :: Effect Unit 
main = do 
     WS.runServer 1337 $ application []

application :: Array Int -> WS.ServerApp
application _ pending = launchAff_ do 
    Console.log "Running"
    conn <- WS.acceptRequest pending WS.defaultAcceptRequest
    WS.withPingThread conn 1000.00 (pure unit) do 
        (d :: String) <- WS.receiveData conn 
        case d of
            ".exit" -> Console.log "Existing..." *> Ws.sendClose conn ""
            _       -> Console.log $ "Received: " <> d

