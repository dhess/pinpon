module Network.PinPon.Util
  ( recordTypeJSONOptions
  , recordTypeSwaggerOptions
  , encodeText
  ) where

import Protolude
import Control.Lens ((^.), strict)
import Data.Aeson (ToJSON, encode)
import Data.Aeson.Types (Options(..), camelTo2, defaultOptions)
import Data.Text (Text)
import Data.Text.Strict.Lens (utf8)
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

encodeText :: ToJSON a => a -> Text
encodeText a = encode a ^. strict . utf8
