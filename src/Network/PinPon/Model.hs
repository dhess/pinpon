{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.PinPon.Model
  ( -- * Types
    AppDb(..)
  , Service(..)
  , allServices
  , Topic(..)
  , TopicId
  , TopicName

    -- * Lenses
  , topicService
  , topicId
  , topics
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
import Data.Swagger
       (ToSchema(..), description, example, genericDeclareNamedSchema,
        schema)
import Data.Text (Text)
import GHC.Generics (Generic)

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

type TopicId = Text
type TopicName = Text

-- | Notification topics.
data Topic =
  Topic {_topicService :: !Service
        ,_topicId :: !TopicId}
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

data AppDb =
  AppDb {_topics :: !(Map TopicName Topic)}
  deriving (Generic,Show,Read,Eq,Data,Typeable)

makeClassy ''AppDb

$(deriveSafeCopy 0 'base ''AppDb)


-- acid-state interface

addTopic :: TopicName -> Topic -> Update AppDb ()
addTopic n t = modifying topics $ at n ?~ t

getTopic :: TopicName -> Query AppDb (Maybe Topic)
getTopic n = views topics $ preview (ix n)

allTopics :: Query AppDb [(TopicName, Topic)]
allTopics = views topics Map.toList

$(makeAcidic ''AppDb ['addTopic
                     ,'getTopic
                     ,'allTopics])
