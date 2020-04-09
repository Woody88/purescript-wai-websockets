module Network.WebSockets.Connection.Options where

import Prelude

import Effect (Effect)

newtype ConnectionOptions = ConnectionOptions
    { connectionOnPong :: Effect Unit
    }

defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
    { connectionOnPong: pure unit  
    }
