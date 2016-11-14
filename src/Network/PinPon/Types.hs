{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Types
  ( -- * Types
    App(..)
  , Config(..)
  , Service(..)
  , Topic(..)

    -- * Lenses
  , awsEnv
  , keyToTopic
  , service
  , topicName
  ) where

import Control.Lens
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (MonadResource(..), ResourceT)
import Data.Aeson.Types
       (FromJSON(..), Options(..), ToJSON(..), camelTo2, defaultOptions,
        genericParseJSON, genericToEncoding, genericToJSON)
import Data.Map.Strict (Map)
import qualified Data.Swagger as Swagger (SchemaOptions(..))
import Data.Swagger
       (ToSchema(..), defaultSchemaOptions, description, example,
        genericDeclareNamedSchema, schema)
import Data.Text (Text)
import GHC.Generics
import Network.AWS (Env, HasEnv(..))
import Servant (ServantErr)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Swagger.Schema.Validation

sumTypeJSONOptions :: Options
sumTypeJSONOptions =
  defaultOptions {fieldLabelModifier = drop 1
                 ,constructorTagModifier = camelTo2 '_'}

sumTypeSwaggerOptions :: Swagger.SchemaOptions
sumTypeSwaggerOptions =
  defaultSchemaOptions {Swagger.fieldLabelModifier = drop 1
                       ,Swagger.constructorTagModifier = camelTo2 '_'}

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

-- | Notification topics.
data Topic =
  Topic {_service :: !Service
        ,_topicName :: !Text}
  deriving (Show,Generic,Eq)

makeClassy ''Topic

instance ToJSON Topic where
  toJSON = genericToJSON sumTypeJSONOptions
  toEncoding = genericToEncoding sumTypeJSONOptions
instance FromJSON Topic where
  parseJSON = genericParseJSON sumTypeJSONOptions

-- $
-- >>> validateToJSON $ Topic AWS "test1"
-- []
-- >>> validateToJSON $ Topic FCM "test2"
-- []
instance ToSchema Topic where
  declareNamedSchema proxy = genericDeclareNamedSchema sumTypeSwaggerOptions proxy
    & mapped.schema.description ?~ "A topic"
    & mapped.schema.example ?~ toJSON (Topic AWS "test_topic_1")

data Config =
  Config {_awsEnv :: Env
         ,_keyToTopic :: Map Text Topic}

makeClassy ''Config

instance HasEnv Config where
  environment = awsEnv

newtype App a =
  App {runApp :: ReaderT Config (ResourceT (ExceptT ServantErr IO)) a}
  deriving (Functor,Applicative,Monad,MonadBase IO,MonadError ServantErr,MonadCatch,MonadThrow,MonadReader Config,MonadIO,MonadResource)
