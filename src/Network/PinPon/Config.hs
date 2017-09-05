{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Config
  ( -- * Types
    App(..)
  , Config(..)
  , createConfig
  , Platform(..)

    -- * Lenses
  , env
  , arn
  , platforms
  ) where

import Control.Lens
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.Trans.AWS
       (Region, Credentials, newEnv, runResourceT, runAWST, send)
import Control.Monad.Trans.Resource (MonadResource(..), ResourceT)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set (empty)
import Data.Text (Text)
import Network.AWS (Env, HasEnv(..))
import Network.AWS.SNS (createTopic, ctrsTopicARN)
import Servant (Handler, ServantErr)

-- | @pinpon@ can deliver platform-specific messages via Amazon SNS
-- for the following platforms.
--
-- Note that, though it is not listed here, @pinpon@ supports delivery
-- via SNS to email addresses. This support is always enabled.
--
-- Platforms not specifically supported by @pinpon@ will still receive
-- notifications, but those notifications will not have any
-- platform-specific features; clients on those platforms will receive
-- only the notification message text. (See
-- "Network.PinPon.Notification.Notification".)
data Platform
  = APNS        -- ^ Apple's APN service
  | APNSSandbox -- ^ Apple's APN sandbox service
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Config = Config
  { _env :: !Env
  , _arn :: !Text
  , _platforms :: Set Platform
  }

makeClassy ''Config

instance HasEnv Config where
  environment = env

-- | Given an AWS 'Region' and a 'Credentials' descriptor, create an
-- SNS topic with the given 'Text' name, and return a @pinpon@ server
-- 'Config' for the topic. Notifications sent to the @pinpon@ server
-- using this 'Config' will be relayed to the SNS topic for delivery
-- to the topic's subscribers.
--
-- The initial set of platforms in the newly-created 'Config' is
-- empty. If you want to send platform-specific messages, add the
-- desired platform(s) to the 'Config's platform set after creating
-- the 'Config'.
--
-- Note that if the topic already exists with the given credentials
-- and region, then this operation is idempotent.
createConfig :: Region -> Credentials -> Text -> IO Config
createConfig region credentials topicName =
  do e <- newEnv credentials
     let enviro = e & envRegion .~ region
     topic <- runResourceT . runAWST enviro $
        send $ createTopic topicName
     return Config { _env = enviro
                   , _arn = fromJust $ topic ^. ctrsTopicARN
                   , _platforms = Set.empty
                   }
newtype App a =
  App {runApp :: ReaderT Config (ResourceT Handler) a}
  deriving (Functor,Applicative,Monad,MonadBase IO,MonadError ServantErr,MonadCatch,MonadThrow,MonadReader Config,MonadIO,MonadResource)
