module Network.PinPon.Util
  ( sumTypeJSONOptions
  , sumTypeSwaggerOptions
  ) where

import Data.Aeson.Types (Options(..), camelTo2, defaultOptions)
import qualified Data.Swagger as Swagger (SchemaOptions(..))
import Data.Swagger (defaultSchemaOptions)

sumTypeJSONOptions :: Options
sumTypeJSONOptions =
  defaultOptions {fieldLabelModifier = drop 1
                 ,constructorTagModifier = camelTo2 '_'}

sumTypeSwaggerOptions :: Swagger.SchemaOptions
sumTypeSwaggerOptions =
  defaultSchemaOptions {Swagger.fieldLabelModifier = drop 1
                       ,Swagger.constructorTagModifier = camelTo2 '_'}
