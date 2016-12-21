{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Types
  ( -- * Types
    App(..)
  , Config(..)

    -- * Lenses
  , awsEnv
  , appDb
  ) where

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.Trans.Resource (MonadResource(..), ResourceT)
import Network.AWS (Env, HasEnv(..))
import Servant (ServantErr)

import Network.PinPon.Model (AppDb)

data Config =
  Config {_awsEnv :: Env
         ,_appDb :: TVar AppDb}

makeClassy ''Config

instance HasEnv Config where
  environment = awsEnv

newtype App a =
  App {runApp :: ReaderT Config (ResourceT (ExceptT ServantErr IO)) a}
  deriving (Functor,Applicative,Monad,MonadBase IO,MonadError ServantErr,MonadCatch,MonadThrow,MonadReader Config,MonadIO,MonadResource)
