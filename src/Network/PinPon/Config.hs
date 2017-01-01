{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Config
  ( -- * Types
    App(..)
  , Config(..)
  , createConfig

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
import Control.Monad.Trans.AWS
       (Region, Credentials, newEnv, runResourceT, runAWST, send)
import Control.Monad.Trans.Resource (MonadResource(..), ResourceT)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.AWS (Env, HasEnv(..))
import Network.AWS.SNS (createTopic, ctrsTopicARN)
import Servant (ServantErr)

data Config =
  Config {_env :: !Env
         ,_arn :: !Text}

makeClassy ''Config

instance HasEnv Config where
  environment = env

-- | Given an AWS 'Region' and a 'Credentials' descriptor, create an
-- SNS topic with the given 'Text' name, and return a @pinpon@ server
-- 'Config' for the topic. Notifications sent to the @pinpon@ server
-- using this 'Config' will be relayed to the SNS topic.
--
-- Note that if the topic already exists with the given credentials
-- and region, then this operation is idempotent.
createConfig :: Region -> Credentials -> Text -> IO Config
createConfig region credentials topicName =
  do enviro <- newEnv region credentials
     topic <- runResourceT . runAWST enviro $
        send $ createTopic topicName
     return Config {_env = enviro
                   ,_arn = fromJust $ topic ^. ctrsTopicARN }
newtype App a =
  App {runApp :: ReaderT Config (ResourceT (ExceptT ServantErr IO)) a}
  deriving (Functor,Applicative,Monad,MonadBase IO,MonadError ServantErr,MonadCatch,MonadThrow,MonadReader Config,MonadIO,MonadResource)
