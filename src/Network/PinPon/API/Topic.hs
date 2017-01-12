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
import Data.Swagger
       (ToSchema(..), description, example, genericDeclareNamedSchema,
        schema)
import Data.Text (Text)
import GHC.Generics
import Lucid
       (ToHtml(..), HtmlT, doctypehtml_, head_, title_, body_)
import Network.AWS.SNS.Publish (publish, pSubject, pTargetARN)
import Servant ((:>), JSON, Post, ReqBody, ServerT)
import Servant.HTML.Lucid (HTML)

import Network.PinPon.AWS (runSNS)
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
    & mapped.schema.example ?~
        toJSON (Notification "Ring! Ring" "Someone is ringing the doorbell!")

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
  "topic" :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

topicServer :: ServerT TopicAPI App
topicServer =
  notify
  where
    notify :: Notification -> App Notification
    notify n =
      do arn <- asks _arn
         void $ runSNS $ publish (_body n)
                                  & pSubject ?~ _subject n
                                  & pTargetARN ?~ arn
         return n
