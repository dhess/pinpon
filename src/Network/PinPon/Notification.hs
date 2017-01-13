{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Notification
  ( Notification(..)
  , body
  , title
  ) where

import Control.Lens ((&), (?~), mapped, makeLenses)
import Control.Monad (void)
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), genericParseJSON, genericToEncoding,
        genericToJSON)
import Data.Text (Text)
import Data.Swagger
       (ToSchema(..), description, example, genericDeclareNamedSchema,
        schema)
import GHC.Generics
import Lucid
       (ToHtml(..), HtmlT, doctypehtml_, head_, title_, body_)

import Network.PinPon.Util
       (recordTypeJSONOptions, recordTypeSwaggerOptions)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Swagger.Schema.Validation

data Notification = Notification
  { _title :: Text
  , _body :: Text
  } deriving (Show, Generic)

makeLenses ''Notification

instance ToJSON Notification where
  toJSON = genericToJSON recordTypeJSONOptions
  toEncoding = genericToEncoding recordTypeJSONOptions
instance FromJSON Notification where
  parseJSON = genericParseJSON recordTypeJSONOptions

-- $
-- >>> validateToJSON $ Notification "Hi" "Test"
-- []
instance ToSchema Notification where
  declareNamedSchema proxy = genericDeclareNamedSchema recordTypeSwaggerOptions proxy
    & mapped.schema.description ?~ "A notification"
    & mapped.schema.example ?~
        toJSON (Notification "Ring! Ring" "Someone is ringing the doorbell!")

notificationDocument
  :: Monad m
  => HtmlT m a -> HtmlT m a -> HtmlT m a
notificationDocument t b =
  doctypehtml_ $ do
    void $ head_ $ title_ t
    body_ b

instance ToHtml Notification where
  toHtml (Notification t b) =
    notificationDocument (toHtml t) (toHtml b)
  toHtmlRaw = toHtml
