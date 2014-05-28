{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Airbrake (
    -- *** Notifying
    notify, notifyReq,

    -- *** Wrapping errors
    toError, Error (..),

    -- *** Configuration building
    APIKey, Environment,
    airbrakeConf, defaultApiEndpoint,
    AirbrakeConf (..),
    Server (..)
) where

import qualified Airbrake.WebRequest as W
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.String
import qualified Data.Text as T (Text)
import Data.Text (pack)
import Data.Typeable (typeOf)
import Data.Version
import qualified Paths_airbrake as P
import Prelude hiding (error)
import Network.HTTP.Conduit
import qualified Network.Wai as Wai
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Renderer.Utf8

type APIKey = String
type Environment = String

data Error = Error
           { errorType :: T.Text
           , errorDescription :: T.Text
           }

-- | Information to use when communicating with Airbrake.
data AirbrakeConf = AirbrakeConf
                  { acApiEndpoint :: String
                  , acApiKey :: APIKey
                  , acServer :: Server
                  }

-- | Metadata about the server.
data Server = Server
            { serverEnvironment :: Environment
            , serverAppVersion :: Maybe Version
            , serverRoot :: Maybe FilePath
            }

-- | @"http:\/\/api.airbrake.io\/notifier_api\/v2\/notices"@
defaultApiEndpoint :: String
defaultApiEndpoint = "http://api.airbrake.io/notifier_api/v2/notices"

airbrakeConf :: APIKey -> Environment -> AirbrakeConf
airbrakeConf k env =
    AirbrakeConf defaultApiEndpoint k (Server env Nothing Nothing)

notifyReqM :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, W.WebRequest req)
           => AirbrakeConf -> Maybe req -> Error -> m ()
notifyReqM conf req e = do
    let report = buildReport conf req e
    req' <- parseUrl (acApiEndpoint conf)
    let rq = req' { requestBody = RequestBodyLBS report, method = "POST" }
    _ <- withManager (httpLbs rq)
    return ()

-- | Notify Airbrake of an exception.
notify :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
       => AirbrakeConf -> Error -> m ()
notify conf = notifyReqM conf (Nothing :: Maybe Wai.Request)

-- | Notify Airbrake of an exception, providing request metadata.
notifyReq :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, W.WebRequest req)
          => AirbrakeConf -> req -> Error -> m ()
notifyReq conf req = notifyReqM conf (Just req)

-- | Convert any 'Exception' to an 'Error'.
toError :: Exception e => e -> Error
toError (toException -> SomeException e) =
    Error (pack (show (typeOf e))) (pack (show e))

buildReport :: W.WebRequest a => AirbrakeConf -> Maybe a -> Error -> ByteString
buildReport conf req err = renderMarkup $ do
    preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    notice ! nversion "2.3" $ do
        api_key . toMarkup $ acApiKey conf

        notifier $ do
            name "airbrake"
            version . toMarkup $ showVersion P.version
            url "http://hackage.haskell.org/package/airbrake"

        error $ do
            class_ (toMarkup (errorType err))
            message (toMarkup (errorDescription err))
            backtrace $
                line ! file __FILE__ ! number (toValue (__LINE__ :: Integer))

        forM_ req $ \ r -> request $ do
            url (toMarkup . show $ W.url r)
            forM_ (W.route r) $ \ rt -> component (toMarkup rt)
            forM_ (W.action r) $ \ act -> action (toMarkup act)
            cgi_data . forM_ (W.otherVars r) $ \ (k, v) ->
                var ! key (toValue k) $ toMarkup v

        let serv = acServer conf
        server_environment $ do
            environment_name . toMarkup $ serverEnvironment serv
            forM_ (serverAppVersion serv) $ \ v ->
                app_version (toMarkup $ showVersion v)

            forM_ (serverRoot serv) $ \ v ->
                project_root (toMarkup v)
    where
        notice = Parent "notice" "<notice" "</notice>"
        name = Parent "name" "<name" "</name>"
        notifier = Parent "notifier" "<notifier" "</notifier>"
        api_key = Parent "api-key" "<api-key" "</api-key>"
        version = Parent "version" "<version" "</version>"
        url = Parent "url" "<url" "</url>"
        class_ = Parent "class" "<class" "</class>"
        error = Parent "error" "<error" "</error>"
        message = Parent "message" "<message" "</message>"
        backtrace = Parent "backtrace" "<backtrace" "</backtrace>"
        line = Leaf "line" "<line" " />"
        file = attribute "file" " file=\""
        number = attribute "number" " number=\""
        server_environment = Parent "server-environment" "<server-environment"
                                 "</server-environment>"
        environment_name = Parent "environment-name" "<environment-name"
                               "</environment-name>"
        app_version = Parent "app-version" "<app-version" "</app-version>"
        project_root = Parent "project-root" "<project-root" "</project-root>"
        request = Parent "request" "<request" "</request>"
        cgi_data = Parent "cgi-data" "<cgi-data" "</cgi-data>"
        action = Parent "action" "<action" "</action>"
        component = Parent "component" "<component" "</component>"
        var = Parent "var" "<var" "</var>"
        key = attribute "key" " key=\""
        nversion = attribute "version" " version=\""
