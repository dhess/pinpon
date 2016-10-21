{-# LANGUAGE TypeOperators #-}

module Network.PinPon.API
  ( -- * Types
    PinPonAPI

    -- * Servant / WAI functions
  , app
  , pinPonAPI
  , server
  )
  where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Network.Wai (Application)
import Servant
       ((:~>)(..), Proxy(..), Server, ServantErr(..), enter, serve)

import Network.PinPon.API.Notify (NotifyAPI, notifyServer)
import Network.PinPon.Config (App(..), Config(..))

-- | Combine all of the various individual service APIs into a single
-- API type.
type PinPonAPI = NotifyAPI

pinPonAPI :: Proxy PinPonAPI
pinPonAPI = Proxy

appToExceptT :: Config -> App :~> ExceptT ServantErr IO
appToExceptT config = Nat $ \a -> runResourceT (runReaderT (runApp a) config)

-- | A Servant 'Server' which serves the 'PinPonAPI' on the given
-- 'Config'.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'PinPonAPI'.
server :: Config -> Server PinPonAPI
server config = enter (appToExceptT config) notifyServer

-- | A WAI 'Network.Wai.Application' which runs the service, using the
-- given 'Config'.
app :: Config -> Application
app = serve pinPonAPI . server
