{-# LANGUAGE FlexibleContexts #-}

module Network.PinPon.AWS
  ( runSNS
  ) where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.AWS (runAWST, send)
import Control.Monad.Trans.Resource (MonadResource)
import Network.AWS.SNS.Publish (Publish, PublishResponse)

import Network.PinPon.Config (Config(..))

runSNS :: (MonadReader Config m, MonadResource m, MonadCatch m) => Publish -> m PublishResponse
runSNS publish =
  do env <- asks _awsEnv
     runAWST env $ send publish

