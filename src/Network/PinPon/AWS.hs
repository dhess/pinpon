{-# LANGUAGE FlexibleContexts #-}

module Network.PinPon.AWS
  ( runAWS
  ) where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.AWS (AWST', runAWST)
import Control.Monad.Trans.Resource (MonadResource)
import Network.AWS (Env)

import Network.PinPon.Config (Config(..))

runAWS :: (MonadReader Config m, MonadResource m) => AWST' Env m a -> m a
runAWS action =
  do env <- asks _awsEnv
     runAWST env action

