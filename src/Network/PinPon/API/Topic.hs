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

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar', readTVar)
import Control.Lens ((&), (^.), (?~), at, mapped, over)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
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
import Network.AWS.SNS (createTopic, ctrsTopicARN)
import Network.AWS.SNS.Publish (publish, pSubject, pTargetARN)
import Servant
       ((:>), (:<|>)(..), Capture, Get, JSON, Post, ReqBody, ServerT,
        ServantErr(..), err404, err501, throwError)
import Servant.HTML.Lucid (HTML)

import Network.PinPon.AWS (runSNS)
import Network.PinPon.Model (Service(..), Topic(..), service, topicName, topics)
import Network.PinPon.Types
       (App(..), Config(..))
import Network.PinPon.Util
       (recordTypeJSONOptions, recordTypeSwaggerOptions)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Swagger.Schema.Validation

data Notification =
  Notification {_subject :: Text
               ,_body :: Text}
  deriving (Show,Generic)

instance ToJSON Notification where
  toJSON = genericToJSON recordTypeJSONOptions
  toEncoding = genericToEncoding recordTypeJSONOptions
instance FromJSON Notification where
  parseJSON = genericParseJSON recordTypeJSONOptions

-- $
-- >>> validateToJSON $ Notification "Hi" "Test"
-- []
instance ToSchema Notification where
  declareNamedSchema proxy = genericDeclareNamedSchema recordTypeSwaggerOptions proxy
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
  "topic" :> ReqBody '[JSON] Topic :> Post '[JSON] Text :<|>
  "topic" :> Capture "key" Text :> "notification" :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

topicServer :: ServerT TopicAPI App
topicServer =
  allTopics :<|>
  newTopic :<|>
  notify
  where
    allTopics :: App [(Text, Topic)]
    allTopics =
      do tvar <- asks _appDb
         liftIO $ atomically $
           do db <- readTVar tvar
              return $! Map.toList (db ^. topics)

    newTopic :: Topic -> App Text
    newTopic topic = newTopic' $ topic ^. service
      where
        newTopic' :: Service -> App Text
        newTopic' AWS =
          do result <- runSNS $ createTopic $ topic ^. topicName
             case result ^. ctrsTopicARN of
               Just arn ->
                 do tvar <- asks _appDb
                    liftIO $
                      atomically $
                        modifyTVar' tvar $
                          over topics $ at (topic ^. topicName) ?~ Topic AWS arn
                    return arn
               Nothing ->
                 throwError $
                   err501 { errBody = "AWS returned a success code, but no topic ARN" }
        newTopic' FCM =
          throwError $
            err501 { errBody = "Firebase Cloud Messaging currently unsupported" }

    notify :: Text -> Notification -> App Notification
    notify k n =
      do tvar <- asks _appDb
         t <- liftIO $ atomically $
           do db <- readTVar tvar
              return $ Map.lookup k (db ^. topics)
         case t of
           Nothing -> throwError $ err404 { errBody = "key not found" }
           Just (Topic AWS arn) ->
             do void $ runSNS $ publish (_body n)
                                         & pSubject ?~ _subject n
                                         & pTargetARN ?~ arn
                return n
           Just (Topic FCM _) ->
             throwError $
               err501 { errBody = "Firebase Cloud Messaging currently unsupported" }
