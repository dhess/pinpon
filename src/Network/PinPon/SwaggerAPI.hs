{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.SwaggerAPI
  ( -- * Types
    API

    -- * Servant / WAI functions
  , app
  , api
  , server

    -- * Swagger
  , pinPonSwagger

     -- * Convenience functions
  , writeSwaggerJSON
  )
  where

import Protolude
import Control.Lens ((&), (.~), (?~))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Swagger
       (Swagger, URL(..), description, info, license, title, url, version)
import Network.Wai (Application)
import Servant ((:<|>)(..), Proxy(..), Server, serve)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

import qualified Network.PinPon.API as PinPon (API, api, server)
import Network.PinPon.Config (Config(..))

-- | Combine all of the various individual service APIs (plus Swagger
-- support) into a single API type.
type API =
  PinPon.API :<|>
  SwaggerSchemaUI "swagger-ui" "swagger.json"

api :: Proxy API
api = Proxy

-- | A Servant 'Server' which serves the 'API' (including the Swagger
-- schema) on the given 'Config'.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'API'.
server :: Config -> Server API
server config = PinPon.server config :<|> swaggerSchemaUIServer pinPonSwagger

-- | A WAI 'Network.Wai.Application' which runs the service, using the
-- given 'Config'.
app :: Config -> Application
app = serve api . server

pinPonSwagger :: Swagger
pinPonSwagger = toSwagger PinPon.api
  & info.title .~ "PinPon API"
  & info.version .~ "0.1"
  & info.description ?~ "A simple Internet-enabled doorbell notificaion service"
  & info.license ?~ ("BSD3" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")

writeSwaggerJSON :: IO ()
writeSwaggerJSON = C8.writeFile "swagger.json" (encodePretty pinPonSwagger)
