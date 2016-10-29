{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.Swagger
  ( -- * Types
    PinPonAPI

    -- * Servant / WAI functions
  , app
  , pinPonAPI
  , server

    -- * Swagger
  , pinPonSwagger

     -- * Convenience functions
  , writeSwaggerJSON
  )
  where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Swagger
       (Swagger, URL(..), description, info, license, title, url, version)
import Network.Wai (Application)
import Servant ((:<|>)(..), Proxy(..), Server, serve)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

import qualified Network.PinPon.API as PinPon (PinPonAPI, pinPonAPI, server)
import Network.PinPon.Config (Config(..))

-- | Combine all of the various individual service APIs (plus Swagger
-- support) into a single API type.
type PinPonAPI =
  PinPon.PinPonAPI :<|>
  SwaggerSchemaUI "swagger-ui" "swagger.json"

pinPonAPI :: Proxy PinPonAPI
pinPonAPI = Proxy

-- | A Servant 'Server' which serves the 'PinPonAPI' (including the
-- Swagger schema) on the given 'Config'.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'PinPonAPI'.
server :: Config -> Server PinPonAPI
server config = PinPon.server config :<|> swaggerSchemaUIServer pinPonSwagger

-- | A WAI 'Network.Wai.Application' which runs the service, using the
-- given 'Config'.
app :: Config -> Application
app = serve pinPonAPI . server

pinPonSwagger :: Swagger
pinPonSwagger = toSwagger PinPon.pinPonAPI
  & info.title .~ "PinPon API"
  & info.version .~ "0.1"
  & info.description ?~ "Send notifications to various cloud services"
  & info.license ?~ ("BSD3" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")

writeSwaggerJSON :: IO ()
writeSwaggerJSON = C8.writeFile "swagger.json" (encodePretty pinPonSwagger)
