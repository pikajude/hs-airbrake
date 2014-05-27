{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Airbrake (
    notify,
    notifyReq,
    AirbrakeConf (..),
    Server (..)
) where

import Airbrake.DSL
import qualified Airbrake.WebRequest as W
import Control.Exception
import Control.Monad
import Data.String
import Data.Text.Lazy (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Typeable (typeOf)
import Data.Version
import qualified Paths_hairbrake as P
import Prelude hiding (error)
import Network.HTTP.Conduit
import qualified Network.Wai as Wai
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Renderer.Utf8

-- | Information to use when communicating with Airbrake.
data AirbrakeConf = AirbrakeConf
                  { acApiEndpoint :: String
                  , acApiKey :: String
                  , acServer :: Server
                  }

-- | Metadata about the server.
data Server = Server
            { serverEnvironment :: String
            , serverAppVersion :: Maybe Version
            , serverRoot :: Maybe FilePath
            }

defaultApiEndpoint :: String
defaultApiEndpoint = "http://api.airbrake.io/notifier_api/v2/notices"

defaultEnvironment :: String
defaultEnvironment = "development"

airbrakeConf :: String -> AirbrakeConf
airbrakeConf key =
    AirbrakeConf defaultApiEndpoint key
        (Server defaultEnvironment Nothing Nothing)

-- | Notify Airbrake of an exception, providing request metadata along with it.
notifyReq :: (W.WebRequest req, Exception e) => AirbrakeConf -> req -> e -> IO ()
notifyReq conf req e = notifyReqM conf (Just req) (toException e)

notifyReqM conf req e = do
    let report = buildReport conf req (toException e)
    print report
    req' <- parseUrl (acApiEndpoint conf)
    let req = req' { requestBody = RequestBodyLBS report, method = "POST" }
    res <- withManager (httpLbs req)
    print res

-- | Notify Airbrake of an exception.
notify :: Exception e => AirbrakeConf -> e -> IO ()
notify conf e = notifyReqM conf (Nothing :: Maybe Wai.Request) (toException e)

sh = fromString . show

buildReport conf req (SomeException e) = renderMarkup $ do
    preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    notice ! nversion "2.3" $ do
        api_key . fromString $ acApiKey conf

        notifier $ do
            name "hairbrake"
            version . fromString $ showVersion P.version
            url "http://hackage.haskell.org/package/hairbrake"

        error $ do
            class_ . sh $ typeOf e
            message $ sh e
            backtrace $
                line ! file __FILE__ ! number (sh (__LINE__ :: Integer))

        m req $ \ r -> request $ do
            url (fromString $ W.url r)
            m (W.route r) $ \ rt -> component (fromString rt)
            m (W.action r) $ \ act -> action (fromString act)
            cgi_data . forM_ (W.otherVars r) $ \ (k, v) ->
                var ! key (fromString k) $ fromString v

        let serv = acServer conf
        server_environment $ do
            environment_name . fromString $ serverEnvironment serv
            m (serverAppVersion serv) $ \ v ->
                app_version (fromString $ showVersion v)

            m (serverRoot serv) $ \ v ->
                project_root (fromString v)

m Nothing _ = return ()
m (Just x) f = f x
