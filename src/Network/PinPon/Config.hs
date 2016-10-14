{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Config
  ( -- * Types
    App(..)
  , Config(..)

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
import Control.Monad.Trans.AWS (AWST', Env, HasEnv(..))
import Control.Monad.Trans.Resource (MonadResource(..), ResourceT)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Servant (ServantErr)

data Config =
  Config {_awsEnv :: Env
         ,_keyToTopic :: Map Text Text}

makeClassy ''Config

instance HasEnv Config where
  environment = awsEnv

newtype App a =
  App {runApp :: AWST' Config (ResourceT (ExceptT ServantErr IO)) a}
  deriving (Functor,Applicative,Monad,MonadBase IO,MonadError ServantErr,MonadCatch,MonadThrow,MonadReader Config,MonadIO,MonadResource)
