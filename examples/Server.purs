module Examples.Server where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class.Console as Console
import Network.WebSockets.Connection (acceptRequest, defaultAcceptRequest, receiveData, sendClose, sendTextData, withPingThread) as WS
import Network.WebSockets.Server (ServerApp, runServer) as WS

main :: Effect Unit 
main = do 
     WS.runServer 1337 $ application []

application :: Array Int -> WS.ServerApp
application _ pending = launchAff_ do 
    Console.log "Running"
    eConn <- attempt $ WS.acceptRequest pending WS.defaultAcceptRequest
    case eConn of 
        Left e     -> Console.log $ "myerror: " <> show e 
        Right conn -> 
            WS.withPingThread conn 1000.00 (pure unit) do 
                (d :: String) <- WS.receiveData conn 
                case d of
                    ".exit" -> Console.log "Existing..." *> WS.sendClose conn "closing"
                    _       -> (Console.log $ "Received: " <> d) *> WS.sendTextData conn "hello back"

