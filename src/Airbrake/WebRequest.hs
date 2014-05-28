{-# LANGUAGE OverloadedStrings #-}

-- | Class for extracting metadata from HTTP request types that come from
-- different libraries.
module Airbrake.WebRequest (
    WebRequest (..)
) where

import Data.ByteString.UTF8 (toString)
import Data.Maybe
import qualified Network.Wai as Wai
import Network.URI

class WebRequest a where
    -- | The request URL.
    url :: a -> URI

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
    url req = case parseURI uriS of
                  Just u -> u
                  Nothing -> error "Failure producing URI from wai request."
        where
            uriS = (if Wai.isSecure req then "https://" else "http://")
                ++ show (Wai.remoteHost req)
                ++ toString (Wai.rawPathInfo req)
                ++ toString (Wai.rawQueryString req)

    route _ = Nothing
    action _ = Nothing

    otherVars req = catMaybes
        [ k "Host" "HTTP_HOST"
        , k "User-Agent" "HTTP_USER_AGENT"
        , k "Referer" "HTTP_REFERER"
        , k "Cookie" "HTTP_COOKIE"
        , if Wai.isSecure req then Just ("HTTPS", "on") else Nothing]
        where k hdr key = fmap (\ v -> (key, toString v))
                               (lookup hdr (Wai.requestHeaders req))
