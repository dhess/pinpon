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

import Control.Lens ((&), (?~), mapped)
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), Options(..), camelTo2, defaultOptions,
        genericParseJSON, genericToEncoding, genericToJSON)
import qualified Data.Map.Strict as Map (lookup, toList)
import qualified Data.Swagger as Swagger (SchemaOptions(..))
import Data.Swagger
       (ToSchema(..), defaultSchemaOptions, description, example,
        genericDeclareNamedSchema, schema)
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

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Swagger.Schema.Validation

data Notification =
  Notification {_subject :: Text
               ,_body :: Text}
  deriving (Show,Generic)

localOptions :: Options
localOptions =
  defaultOptions {fieldLabelModifier = drop 1
                 ,constructorTagModifier = camelTo2 '_'}

instance ToJSON Notification where
  toJSON = genericToJSON localOptions
  toEncoding = genericToEncoding localOptions
instance FromJSON Notification where
  parseJSON = genericParseJSON localOptions

localSchemaOptions :: Swagger.SchemaOptions
localSchemaOptions =
  defaultSchemaOptions {Swagger.fieldLabelModifier = drop 1
                       ,Swagger.constructorTagModifier = camelTo2 '_'}

-- $
-- >>> validateToJSON $ Notification "Hi" "Test"
-- []
instance ToSchema Notification where
  declareNamedSchema proxy = genericDeclareNamedSchema localSchemaOptions proxy
    & mapped.schema.description ?~ "A notification"
    & mapped.schema.example ?~ toJSON (Notification "Hi from AWS" "Hope you're doing well!")

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
                                         & pSubject ?~ _subject n
                                         & pTargetARN ?~ arn
                return n
