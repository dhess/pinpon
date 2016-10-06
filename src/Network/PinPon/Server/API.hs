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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.Server.API
         ( -- * Types
           PinPonAPI
         , Config
         , Key(..)

           -- * Servant / WAI functions
         , app
         , pinPonAPI
         , server
         ) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), Options(..), SumEncoding(TaggedObject),
        defaultOptions, genericToEncoding, genericParseJSON, tagFieldName,
        contentsFieldName)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import GHC.Generics
import Lucid
       (ToHtml(..), HtmlT, doctypehtml_, head_, title_, body_)
import Network.Wai (Application)
import Servant
       ((:>), (:<|>)(..), (:~>)(..), JSON, Get, ReqBody, Post, Proxy(..),
        ServerT, Server, ServantErr, enter, serve)
import Servant.Docs (ToSample(..))
import Servant.HTML.Lucid (HTML)

wrapBody :: Monad m => HtmlT m () -> HtmlT m a -> HtmlT m a
wrapBody title body =
  doctypehtml_ $
    do head_ $
         title_ title
       body_ body

data Key =
  Key {name :: Text}
  deriving (Generic,Show)

instance ToJSON Key where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Key

keyDocument :: Monad m => HtmlT m a -> HtmlT m a
keyDocument = wrapBody "PinPon key"

instance ToHtml Key where
  toHtml (Key key) = keyDocument $ toHtml key
  toHtmlRaw = toHtml

type Config = ()

type PinPonAPI =
  "ring" :> ReqBody '[JSON] Key :> Post '[JSON, HTML] Key

type AppM c m = ReaderT Config (ExceptT ServantErr m)

serverT :: (MonadIO m) => ServerT PinPonAPI (AppM c m)
serverT =
  ring
  where
    ring :: (MonadIO m) => Key -> AppM c m Key
    ring key =
      do config <- ask
         return key

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
