module Wai.WebSockets where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Network.HTTP.Types.Header as H
import Network.Wai.Internal (Request(..))
import Network.Wai.Internal (Request) as Wai

-- | Returns whether or not the given 'Wai.Request' is a WebSocket request.
isWebSocketsReq :: Wai.Request -> Boolean
isWebSocketsReq (Request req) =
    upgradeRequestHeader == (Just $ caseI "websocket")
    where 
        caseI          = CaseInsensitiveString
        requestHeaders = Map.fromFoldable $ req.requestHeaders
        upgradeRequestHeader  = map caseI (Map.lookup H.hUpgrade requestHeaders)
