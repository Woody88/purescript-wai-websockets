module Wai.Websockets.Internal where 

import Prelude 

import Data.Maybe (Maybe(..))
import Effect.Unsafe (unsafePerformEffect)
import Network.Wai (Request(..))
import Data.Vault as V 
import Node.HTTP as HTTP

nodeRequestKey :: V.Key HTTP.Request
nodeRequestKey = unsafePerformEffect V.newKey

withNodeRequest :: forall m. Applicative m => Request -> (HTTP.Request -> m Unit) -> m Unit
withNodeRequest (Request { vault }) run = case V.lookup nodeRequestKey vault of
  Just req -> run req
  Nothing -> pure unit

