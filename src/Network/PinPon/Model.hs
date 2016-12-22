{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.PinPon.Model
  ( -- * Types
    AppDb(..)
  , Service(..)
  , allServices
  , Topic(..)
  , TopicId(..)
  , TopicName(..)

    -- * Lenses
  , topicNameText
  , topicIdText
  , topicService
  , topicName
  , topics

    -- * Functions
  , generateTopicId
  ) where

import Control.Lens
import Data.Acid (Query, Update, makeAcidic)
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), defaultOptions, genericParseJSON,
        genericToEncoding, genericToJSON)
import Data.Data (Data, Typeable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (toList)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.String (IsString)
import Data.Swagger
       (ToParamSchema(..), ToSchema(..), description, example,
        genericDeclareNamedSchema, schema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

import Data.Proxy (Proxy(..))

import Network.PinPon.Util
       (recordTypeJSONOptions, recordTypeSwaggerOptions)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Swagger.Schema.Validation

-- | Supported notification services.
data Service
  = AWS
  | FCM
  deriving (Generic,Show,Read,Eq,Enum,Bounded,Ord,Data,Typeable)

$(deriveSafeCopy 0 'base ''Service)

instance ToJSON Service where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Service where
  parseJSON = genericParseJSON defaultOptions

instance ToSchema Service

-- | The list of all 'Service' values.
allServices :: [Service]
allServices = [(minBound :: Service) ..]

-- | A service-specific name for a subscription/notification topic.
newtype TopicName =
  TopicName {_topicNameText :: Text}
  deriving (Generic,Show,Read,Eq,Ord,Data,Typeable,IsString,ToJSON,FromJSON,FromHttpApiData,ToHttpApiData)

makeLenses ''TopicName
makeWrapped ''TopicName

$(deriveSafeCopy 0 'base ''TopicName)

-- $
-- >>> validateToJSON $ TopicName "test1"
-- []
instance ToParamSchema TopicName where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
instance ToSchema TopicName where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

-- | Notification topics.
data Topic =
  Topic {_topicService :: !Service
        ,_topicName :: !TopicName}
  deriving (Generic,Show,Read,Eq,Data,Typeable)

makeClassy ''Topic

$(deriveSafeCopy 0 'base ''Topic)

instance ToJSON Topic where
  toJSON = genericToJSON recordTypeJSONOptions
  toEncoding = genericToEncoding recordTypeJSONOptions
instance FromJSON Topic where
  parseJSON = genericParseJSON recordTypeJSONOptions

-- $
-- >>> validateToJSON $ Topic AWS "test1"
-- []
-- >>> validateToJSON $ Topic FCM "test2"
-- []
instance ToSchema Topic where
  declareNamedSchema proxy = genericDeclareNamedSchema recordTypeSwaggerOptions proxy
    & mapped.schema.description ?~ "A topic"
    & mapped.schema.example ?~ toJSON (Topic AWS "test_topic_1")

-- | A unique, application-specific identifier for a topic created by
-- a client.
newtype TopicId =
  TopicId { _topicIdText :: Text }
  deriving (Generic,Show,Read,Eq,Ord,Data,Typeable,IsString,ToJSON,FromJSON,FromHttpApiData)

makeLenses ''TopicId
makeWrapped ''TopicId

$(deriveSafeCopy 0 'base ''TopicId)

-- $
-- >>> validateToJSON $ TopicId "test1"
-- []
instance ToParamSchema TopicId where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
instance ToSchema TopicId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

-- | The applicaton database.
data AppDb =
  AppDb {_topics :: !(Map TopicId Topic)}
  deriving (Generic,Show,Read,Eq,Data,Typeable)

makeClassy ''AppDb

$(deriveSafeCopy 0 'base ''AppDb)

-- | Given a 'Topic', create a 'TopicId'.
generateTopicId :: Topic -> TopicId
generateTopicId t = TopicId $ t ^. topicName ^. topicNameText

-- acid-state interface

addTopic :: Topic -> Update AppDb TopicId
addTopic t =
  let tid = generateTopicId t
  in
    do modifying topics $ at tid ?~ t
       return tid

getTopic :: TopicId -> Query AppDb (Maybe Topic)
getTopic tid = views topics $ preview (ix tid)

allTopics :: Query AppDb [(TopicId, Topic)]
allTopics = views topics Map.toList

$(makeAcidic ''AppDb ['addTopic
                     ,'getTopic
                     ,'allTopics])
