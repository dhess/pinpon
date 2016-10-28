{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Lens ((&), (.~), (?~))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Swagger
       (Swagger, URL(..), description, info, license, title, url, version)
import Network.Wai (Application)
import Servant
       ((:>), (:~>)(..), (:<|>)(..), Get, JSON, Proxy(..), Server,
        ServantErr(..), enter, serve)
import Servant.Swagger (toSwagger)

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

-- | A WAI 'Network.Wai.Application' which runs the service (minus the
-- Swagger support), using the given 'Config'.
app :: Config -> Application
app = serve pinPonAPI . server

pinPonSwagger :: Swagger
pinPonSwagger = toSwagger pinPonAPI
  & info.title .~ "PinPon API"
  & info.version .~ "0.1"
  & info.description ?~ "Send notifications to various cloud services"
  & info.license ?~ ("BSD3" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")

writeSwaggerJSON :: IO ()
writeSwaggerJSON = C8.writeFile "swagger.json" (encodePretty pinPonSwagger)
