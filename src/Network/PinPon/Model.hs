{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Model
  ( -- * Types
    AppDb(..)
  , Service(..)
  , allServices
  , Topic(..)

    -- * Lenses
  , service
  , topicName
  , topics
  ) where

import Control.Lens
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), defaultOptions, genericParseJSON,
        genericToEncoding, genericToJSON)
import Data.Map.Strict (Map)
import Data.Swagger
       (ToSchema(..), description, example, genericDeclareNamedSchema,
        schema)
import Data.Text (Text)
import GHC.Generics

import Network.PinPon.Util
       (recordTypeJSONOptions, recordTypeSwaggerOptions)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Swagger.Schema.Validation

-- | Supported notification services.
data Service
  = AWS
  | FCM
  deriving (Show,Generic,Eq,Enum,Bounded,Ord)

instance ToJSON Service where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Service where
  parseJSON = genericParseJSON defaultOptions

instance ToSchema Service

-- | The list of all 'Service' values.
allServices :: [Service]
allServices = [(minBound :: Service) ..]

-- | Notification topics.
data Topic =
  Topic {_service :: !Service
        ,_topicName :: !Text}
  deriving (Show,Generic,Eq)

makeClassy ''Topic

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
  AppDb {_topics :: Map Text Topic}

makeClassy ''AppDb
