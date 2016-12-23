{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.API
  ( -- * Types
    API

    -- * Servant / WAI functions
  , app
  , api
  , server
  )
  where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Resource (runResourceT)
import Network.Wai (Application)
import Servant
       ((:~>)(..), Proxy(..), Server, ServantErr(..), enter, serve)

import Network.PinPon.API.Topic (TopicAPI, topicServer)
import Network.PinPon.Config (App(..), Config(..))

-- | Combine all of the various individual service APIs into a single
-- API type.
type API = TopicAPI

api :: Proxy API
api = Proxy

appToExceptT :: Config -> App :~> ExceptT ServantErr IO
appToExceptT config = Nat $ \a -> runResourceT (runReaderT (runApp a) config)

-- | A Servant 'Server' which serves the 'PinPonAPI' on the given
-- 'Config'.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'API'.
server :: Config -> Server API
server config = enter (appToExceptT config) topicServer

-- | A WAI 'Network.Wai.Application' which runs the service, using the
-- given 'Config'.
app :: Config -> Application
app = serve api . server
