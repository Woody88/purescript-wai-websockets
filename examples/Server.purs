module Examples.Server where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.WebSockets.Connection as WS
import Network.WebSockets.Server (onupgrade)
import Network.WebSockets.Server as WS
import Node.HTTP as HTTP

main :: Effect Unit 
-- main = do 
--     httpsvr <- createHttpServer
--     onerror httpsvr 
--     onupgrade_ httpsvr 
--     HTTP.listen httpsvr {backlog: Nothing, hostname: "localhost",port: 8080} $ Console.log "listening"
main = do 
     WS.runServer 8080 $ application []

application :: Array Int -> WS.ServerApp
application _ pending = launchAff_ do 
    conn <- WS.acceptRequest pending WS.defaultAcceptRequest
    WS.withPingThread conn 30000.00 (Console.log "Pinged") do 
        Console.log "Running"
