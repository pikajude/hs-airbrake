-- | Class for extracting metadata from HTTP request types that come from
-- different libraries.
module Airbrake.WebRequest (
    WebRequest (..)
) where

import Data.ByteString.UTF8 (toString)
import Data.Maybe
import qualified Network.Wai as Wai

class WebRequest a where
    -- | The request URL, which should including the requesting host,
    -- port, full path and query string.
    url :: a -> String

    -- | Current route.
    -- This is a carryover from Rails-style MVC and is optional.
    route :: a -> Maybe String

    -- | Controller action being used.
    -- This is a carryover from Rails-style MVC and is optional.
    action :: a -> Maybe String

    -- | Any other request metadata that you would like to include
    -- (server name, user agent, etc.)
    otherVars :: a -> [(String, String)]

-- | @wai@ requests
instance WebRequest Wai.Request where
    url req = show (Wai.remoteHost req) ++ "/"
           ++ toString (Wai.rawPathInfo req)
           ++ toString (Wai.rawQueryString req)

    route _ = Nothing
    action _ = Nothing

    otherVars req = maybeToList
        $ fmap (\ x -> ("HTTP_HOST", toString x)) (Wai.requestHeaderHost req)
