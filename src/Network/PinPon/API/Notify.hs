{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.API.Notify
         ( -- * Types
           Notification(..)
         , NotifyAPI

           -- * Servant functions
         , notifyServer
         ) where

import Control.Lens ((&), (?~))
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), Options(..), camelTo2, defaultOptions,
        genericParseJSON, genericToEncoding)
import qualified Data.Map.Strict as Map (lookup, toList)
import Data.Text (Text)
import GHC.Generics
import Lucid
       (ToHtml(..), HtmlT, doctypehtml_, head_, title_, body_)
import Network.AWS.SNS.Publish (publish, pSubject, pTargetARN)
import Servant
       ((:>), (:<|>)(..), Capture, Get, JSON, Post, ReqBody, ServerT,
        ServantErr(..), err404, throwError)
import Servant.HTML.Lucid (HTML)

import Network.PinPon.AWS (runSNS)
import Network.PinPon.Config (App(..), Config(..), Service(..))

localOptions :: Options
localOptions =
  defaultOptions {fieldLabelModifier = drop 1
                 ,constructorTagModifier = camelTo2 '_'}

data Notification =
  Notification {_subject :: Text
               ,_body :: Text}
  deriving (Generic)

instance ToJSON Notification where
  toEncoding = genericToEncoding localOptions
instance FromJSON Notification where
  parseJSON = genericParseJSON localOptions

notificationDocument :: Monad m => HtmlT m a -> HtmlT m a -> HtmlT m a
notificationDocument subj body =
  doctypehtml_ $
    do void $ head_ $
                title_ subj
       body_ body

instance ToHtml Notification where
  toHtml (Notification subj body) = notificationDocument (toHtml subj) (toHtml body)
  toHtmlRaw = toHtml

type NotifyAPI =
  "notify" :> Get '[JSON] [(Text, Service)] :<|>
  "notify" :> Capture "key" Text :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

notifyServer :: ServerT NotifyAPI App
notifyServer =
  allEndpoints :<|>
  notify
  where
    allEndpoints :: App [(Text, Service)]
    allEndpoints =
      do m <- asks _keyToTopic
         return $ Map.toList m
    notify :: Text -> Notification -> App Notification
    notify k n =
      do m <- asks _keyToTopic
         case Map.lookup k m of
           Nothing -> throwError $ err404 { errBody = "key not found" }
           Just (AWS arn) ->
             do void $ runSNS $ publish (_body n)
                                         & pSubject ?~ (_subject n)
                                         & pTargetARN ?~ arn
                return n
