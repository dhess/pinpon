{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Config
  ( -- * Types
    App(..)
  , Config(..)

    -- * Lenses
  , env
  , arn
  ) where

import Control.Lens
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.Trans.Resource (MonadResource(..), ResourceT)
import Data.Text (Text)
import Network.AWS (Env, HasEnv(..))
import Servant (ServantErr)

data Config =
  Config {_env :: !Env
         ,_arn :: !Text}

makeClassy ''Config

instance HasEnv Config where
  environment = env

newtype App a =
  App {runApp :: ReaderT Config (ResourceT (ExceptT ServantErr IO)) a}
  deriving (Functor,Applicative,Monad,MonadBase IO,MonadError ServantErr,MonadCatch,MonadThrow,MonadReader Config,MonadIO,MonadResource)
