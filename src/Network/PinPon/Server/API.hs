{-|
Module      : Network.PinPon.Server.API
Description : A REST web service for @pinpon@
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

This module provides a "Servant" REST web service for @pinpon@

See the included <API.md API.md> file for detailed documentation on
the REST service methods and document types.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.Server.API
         ( -- * Types
           PinPonAPI
         , Config(..)
         , Notification(..)

           -- * Servant / WAI functions
         , app
         , pinPonAPI
         , server
         ) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), Options(..), camelTo2, defaultOptions,
        genericParseJSON, genericToEncoding)
import qualified Data.Map.Strict as Map (Map, lookup)
import Data.Text (Text)
import GHC.Generics
import Lucid
       (ToHtml(..), HtmlT, doctypehtml_, head_, title_, body_)
import Network.Wai (Application)
import Servant
       ((:>), (:~>)(..), Capture, JSON, Post, Proxy(..), ReqBody, ServerT,
        Server, ServantErr(..), enter, err404, serve, throwError)
import Servant.HTML.Lucid (HTML)

localOptions :: Options
localOptions =
  defaultOptions {fieldLabelModifier = drop 1
                 ,constructorTagModifier = camelTo2 '_'}

data Config =
  Config {_keyToTopic :: Map.Map Text Text}

data Notification =
  Notification {_body :: Text}
  deriving (Generic)

instance ToJSON Notification where
  toEncoding = genericToEncoding localOptions
instance FromJSON Notification where
  parseJSON = genericParseJSON localOptions

notificationDocument :: Monad m => HtmlT m a -> HtmlT m a
notificationDocument = wrapBody "PinPon Notification"

instance ToHtml Notification where
  toHtml (Notification body) = notificationDocument $ toHtml body
  toHtmlRaw = toHtml

wrapBody :: Monad m => HtmlT m () -> HtmlT m a -> HtmlT m a
wrapBody title body =
  doctypehtml_ $
    do head_ $
         title_ title
       body_ body

type PinPonAPI =
  "notify" :> Capture "key" Text :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

type AppM c m = ReaderT Config (ExceptT ServantErr m)

serverT :: (MonadIO m) => ServerT PinPonAPI (AppM c m)
serverT =
  notify
  where
    notify :: (MonadIO m) => Text -> Notification -> AppM c m Notification
    notify key n =
      do config <- ask
         case Map.lookup key (_keyToTopic config) of
           Nothing -> throwError $ err404 { errBody = "key not found" }
           Just _ -> return n

pinPonAPI :: Proxy PinPonAPI
pinPonAPI = Proxy

serverToEither :: (MonadIO m) => Config -> AppM c m :~> ExceptT ServantErr m
serverToEither config = Nat $ \m -> runReaderT m config

-- | A Servant 'Server' which serves the 'PinPonAPI' on the given
-- 'Config'.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'PinPonAPI'.
server :: Config -> Server PinPonAPI
server config = enter (serverToEither config) serverT

-- | A WAI 'Network.Wai.Application' which runs the service, using the
-- given 'Config'.
app :: Config -> Application
app = serve pinPonAPI . server
