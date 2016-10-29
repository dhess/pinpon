{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.Swagger
  ( -- * Convenience functions
    writeSwaggerJSON
  )
  where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Swagger
       (Swagger, URL(..), description, info, license, title, url, version)
import Servant.Swagger (toSwagger)

import Network.PinPon.API (pinPonAPI)

pinPonSwagger :: Swagger
pinPonSwagger = toSwagger pinPonAPI
  & info.title .~ "PinPon API"
  & info.version .~ "0.1"
  & info.description ?~ "Send notifications to various cloud services"
  & info.license ?~ ("BSD3" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")

writeSwaggerJSON :: IO ()
writeSwaggerJSON = C8.writeFile "swagger.json" (encodePretty pinPonSwagger)
