{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Config
  ( -- * Types
    App(..)
  , Config(..)
  , Service(..)

    -- * Lenses
  , awsEnv
  , keyToTopic
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
       (FromJSON(..), ToJSON(..), defaultOptions, genericParseJSON,
        genericToEncoding)
import Data.Map.Strict (Map)
import Data.Swagger (ToSchema(..))
import Data.Text (Text)
import GHC.Generics
import Network.AWS (Env, HasEnv(..))
import Servant (ServantErr)

-- | Supported notification services and their notification identifier
-- type.
data Service =
  AWS Text
  -- ^ AWS uses a 'Text' "ARN" to identify resources.
  deriving (Show,Generic)

instance ToJSON Service where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Service where
  parseJSON = genericParseJSON defaultOptions

instance ToSchema Service

data Config =
  Config {_awsEnv :: Env
         ,_keyToTopic :: Map Text Service}

makeClassy ''Config

instance HasEnv Config where
  environment = awsEnv

newtype App a =
  App {runApp :: ReaderT Config (ResourceT (ExceptT ServantErr IO)) a}
  deriving (Functor,Applicative,Monad,MonadBase IO,MonadError ServantErr,MonadCatch,MonadThrow,MonadReader Config,MonadIO,MonadResource)
