module Network.PinPon.Util
  ( recordTypeJSONOptions
  , recordTypeSwaggerOptions
  ) where

import Data.Aeson.Types (Options(..), camelTo2, defaultOptions)
import qualified Data.Swagger as Swagger (SchemaOptions(..))
import Data.Swagger (defaultSchemaOptions)

recordTypeJSONOptions :: Options
recordTypeJSONOptions =
  defaultOptions {fieldLabelModifier = drop 1
                 ,constructorTagModifier = camelTo2 '_'}

recordTypeSwaggerOptions :: Swagger.SchemaOptions
recordTypeSwaggerOptions =
  defaultSchemaOptions {Swagger.fieldLabelModifier = drop 1
                       ,Swagger.constructorTagModifier = camelTo2 '_'}
