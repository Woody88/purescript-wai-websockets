module Examples.Server where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Network.WebSockets.Connection (acceptRequest, defaultAcceptRequest, withPingThread) as WS
import Network.WebSockets.Server (ServerApp, runServer) as WS


main :: Effect Unit 
main = do 
     WS.runServer 8080 $ application []

application :: Array Int -> WS.ServerApp
application _ pending = launchAff_ do 
    conn <- WS.acceptRequest pending WS.defaultAcceptRequest
    WS.withPingThread conn 30000.00 (Console.log "Pinged") do 
        Console.log "Running"
