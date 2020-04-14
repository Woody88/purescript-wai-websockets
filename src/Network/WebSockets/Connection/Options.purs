module Network.WebSockets.Connection.Options where

import Prelude

import Effect (Effect)

newtype ConnectionOptions = ConnectionOptions
    { onPong :: Effect Unit
    }

defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
    { onPong: pure unit  
    }
