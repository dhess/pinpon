{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.PinPon.Config
  ( -- * Types
    App(..)
  , Config(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Servant (ServantErr)

data Config =
  Config {_keyToTopic :: Map Text Text}

newtype App a =
  App {runApp :: ReaderT Config (ExceptT ServantErr IO) a}
  deriving (Functor,Applicative,Monad,MonadError ServantErr,MonadReader Config,MonadIO)
