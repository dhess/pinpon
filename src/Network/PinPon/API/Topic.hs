{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.API.Topic
         ( -- * Types
           Notification(..)
         , TopicAPI

           -- * Servant functions
         , topicServer
         ) where

import Control.Lens ((&), (?~), mapped)
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), genericParseJSON, genericToEncoding,
        genericToJSON)
import qualified Data.Map.Strict as Map (lookup, toList)
import Data.Swagger
       (ToSchema(..), description, example, genericDeclareNamedSchema,
        schema)
import Data.Text (Text)
import GHC.Generics
import Lucid
       (ToHtml(..), HtmlT, doctypehtml_, head_, title_, body_)
import Network.AWS.SNS.Publish (publish, pSubject, pTargetARN)
import Servant
       ((:>), (:<|>)(..), Capture, Get, JSON, Post, ReqBody, ServerT,
        ServantErr(..), err404, err501, throwError)
import Servant.HTML.Lucid (HTML)

import Network.PinPon.AWS (runSNS)
import Network.PinPon.Types
       (App(..), Config(..), Service(..), Topic(..))
import Network.PinPon.Util
       (sumTypeJSONOptions, sumTypeSwaggerOptions)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Swagger.Schema.Validation

data Notification =
  Notification {_subject :: Text
               ,_body :: Text}
  deriving (Show,Generic)

instance ToJSON Notification where
  toJSON = genericToJSON sumTypeJSONOptions
  toEncoding = genericToEncoding sumTypeJSONOptions
instance FromJSON Notification where
  parseJSON = genericParseJSON sumTypeJSONOptions

-- $
-- >>> validateToJSON $ Notification "Hi" "Test"
-- []
instance ToSchema Notification where
  declareNamedSchema proxy = genericDeclareNamedSchema sumTypeSwaggerOptions proxy
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

type TopicAPI =
  "topic" :> Get '[JSON] [(Text, Topic)] :<|>
  "topic" :> Capture "key" Text :> "notification" :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

topicServer :: ServerT TopicAPI App
topicServer =
  allEndpoints :<|>
  topic
  where
    allEndpoints :: App [(Text, Topic)]
    allEndpoints =
      do m <- asks _keyToTopic
         return $ Map.toList m
    topic :: Text -> Notification -> App Notification
    topic k n =
      do m <- asks _keyToTopic
         case Map.lookup k m of
           Nothing -> throwError $ err404 { errBody = "key not found" }
           Just (Topic AWS arn) ->
             do void $ runSNS $ publish (_body n)
                                         & pSubject ?~ _subject n
                                         & pTargetARN ?~ arn
                return n
           Just (Topic FCM _) ->
             throwError $
               err501 { errBody = "Firebase Cloud Messaging currently unsupported" }
