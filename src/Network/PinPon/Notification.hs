{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.Notification
  ( -- * Types
    Notification(..)
  , headline
  , message
  , defaultNotification
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

-- | A @pinpon@ doorbell notification.
--
-- @pinpon@ doorbell notifications are sent by @pinpon@ API clients to
-- the server. The server translates these notifications to the
-- format(s) required by the doorbell's subscribers.
data Notification = Notification
  { _headline :: !Text -- ^ A summary of the notification, preferably a single line
  , _message :: !Text  -- ^ A more detailed message (can be multiple lines)
  } deriving (Show, Generic)

makeLenses ''Notification

-- | A default value for 'Notification'.
defaultNotification :: Notification
defaultNotification = Notification "Ring! Ring!" "Someone is ringing the doorbell!"

instance ToJSON Notification where
  toJSON = genericToJSON recordTypeJSONOptions
  toEncoding = genericToEncoding recordTypeJSONOptions
instance FromJSON Notification where
  parseJSON = genericParseJSON recordTypeJSONOptions

-- $
-- >>> validateToJSON $ Notification "Hi" "Test"
-- []
instance ToSchema Notification where
  declareNamedSchema proxy =
    genericDeclareNamedSchema recordTypeSwaggerOptions proxy
      & mapped.schema.description ?~ "A doorbell notification"
      & mapped.schema.example ?~ toJSON defaultNotification

notificationResultDocument :: (Monad m) => HtmlT m a -> HtmlT m a -> HtmlT m a
notificationResultDocument hl msg =
  doctypehtml_ $ do
    void $ head_ $ title_ hl
    body_ msg

instance ToHtml Notification where
  toHtml (Notification hl msg) = notificationResultDocument (toHtml hl) (toHtml msg)
  toHtmlRaw = toHtml
