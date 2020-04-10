module Examples.Client where

import Prelude

import Effect (Effect)
import Effect.AVar as AVar
import Effect.AVar as Avar
import Effect.Aff (error, forkAff, killFiber, launchAff, launchAff_, supervise)
import Effect.Aff.AVar as AvarAff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.WebSockets.Client (runClient) as WS
import Network.WebSockets.Connection (receiveData, sendClose, sendTextData, terminate) as WS
import Node.ReadLine (noCompletion)
import Node.ReadLine as Node

main :: Effect Unit 
main = do 

    bus <- AVar.empty
    fiber <- launchAff do supervise $ WS.runClient "ws://localhost:1337/" $ (supervise <<< app bus) 
    interface <- Node.createConsoleInterface noCompletion
    Node.setPrompt "> " 2 interface
    Node.prompt interface 
    Node.setLineHandler interface \s -> do 
        _ <- Avar.put s bus (\_ -> pure unit)
        if (s == "quit")
        then (Node.close interface *> Console.log "bye")
        else Node.prompt interface

    where 
        app b conn = do 
            fiber <- forkAff do 
                    msg <- WS.receiveData conn 
                    Console.log $ "Received: " <> msg
            s <- AvarAff.take b 
            Console.log $ "you typed: " <> s
            if s == "quit"
            then 
                Console.log "gettingout" *> WS.terminate conn *> killFiber (error "out") fiber
            else do
                WS.sendTextData conn s *> app b conn 
            