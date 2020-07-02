module Network.WebSockets.Http where

import Prelude

import Data.Bifunctor (lmap)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Network.HTTP.Types as H
import Node.HTTP as HTTP

data RequestHead = RequestHead
    { path    :: String 
    , headers :: H.RequestHeaders
    , secure  :: Boolean
    }

-- TODO: find a way to check if connection is secure
mkRequestHead :: HTTP.Request -> Boolean -> RequestHead 
mkRequestHead req secure = 
    RequestHead 
        { path: HTTP.requestURL req 
        , headers
        , secure
        }
    where 
        headers :: H.RequestHeaders
        headers = map (\(Tuple k v) -> Tuple (wrap k) v) $ Object.toUnfoldable $ HTTP.requestHeaders req 