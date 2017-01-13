{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.PinPon.Notification
  ( Notification(..)
  ) where

import Control.Lens ((&), (?~), mapped)
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

notificationDocument subj body =
  doctypehtml_ $ do
    void $ head_ $ title_ subj
    body_ body

instance ToHtml Notification where
  toHtml (Notification subj body) =
    notificationDocument (toHtml subj) (toHtml body)
  toHtmlRaw = toHtml
