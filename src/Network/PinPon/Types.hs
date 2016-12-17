{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Types
  ( -- * Types
    App(..)
  , Config(..)
  , Service(..)
  , allServices
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
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.Trans.Resource (MonadResource(..), ResourceT)
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), defaultOptions, genericParseJSON,
        genericToEncoding, genericToJSON)
import Data.Map.Strict (Map)
import Data.Swagger
       (ToSchema(..), description, example, genericDeclareNamedSchema,
        schema)
import Data.Text (Text)
import GHC.Generics
import Network.AWS (Env, HasEnv(..))
import Servant (ServantErr)

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

data Config =
  Config {_awsEnv :: Env
         ,_keyToTopic :: Map Text Topic}

makeClassy ''Config

instance HasEnv Config where
  environment = awsEnv

newtype App a =
  App {runApp :: ReaderT Config (ResourceT (ExceptT ServantErr IO)) a}
  deriving (Functor,Applicative,Monad,MonadBase IO,MonadError ServantErr,MonadCatch,MonadThrow,MonadReader Config,MonadIO,MonadResource)
