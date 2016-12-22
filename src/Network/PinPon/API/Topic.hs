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
import Network.PinPon.Model
       (Service(..), Topic(..), TopicId(..), TopicName(..),
        generateTopicId, topicNameText, topics)
import Network.PinPon.Config (App(..), Config(..))
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
  "topic" :> Get '[JSON] [(TopicId, Topic)] :<|>
  "topic" :> ReqBody '[JSON] Topic :> Post '[JSON] TopicId :<|>
  "topic" :> Capture "key" TopicId :> "notification" :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

topicServer :: ServerT TopicAPI App
topicServer =
  allTopics :<|>
  newTopic :<|>
  notify
  where
    allTopics :: App [(TopicId, Topic)]
    allTopics =
      do tvar <- asks _appDb
         liftIO $ atomically $
           do db <- readTVar tvar
              return $! Map.toList (db ^. topics)

    newTopic :: Topic -> App TopicId
    newTopic t@(Topic AWS name) =
          do result <- runSNS (createTopic $ name ^. topicNameText)
             case result ^. ctrsTopicARN of
               Just arn ->
                 let tid = generateTopicId t
                 in
                   do tvar <- asks _appDb
                      liftIO $
                        atomically $
                          modifyTVar' tvar $
                            over topics $ at tid ?~ Topic AWS (TopicName arn)
                      return tid
               Nothing ->
                 throwError $
                   err501 { errBody = "AWS returned a success code, but no topic ARN" }
    newTopic (Topic FCM _) =
          throwError $
            err501 { errBody = "Firebase Cloud Messaging currently unsupported" }

    notify :: TopicId -> Notification -> App Notification
    notify tid n =
      do tvar <- asks _appDb
         t <- liftIO $ atomically $
           do db <- readTVar tvar
              return $ Map.lookup tid (db ^. topics)
         case t of
           Nothing -> throwError $ err404 { errBody = "key not found" }
           Just (Topic AWS (TopicName arn)) ->
             do void $ runSNS $ publish (_body n)
                                         & pSubject ?~ _subject n
                                         & pTargetARN ?~ arn
                return n
           Just (Topic FCM _) ->
             throwError $
               err501 { errBody = "Firebase Cloud Messaging currently unsupported" }
