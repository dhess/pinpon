{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.WireTypes.APNS
  ( -- * Apple Push Notification service on-the-wire types.

    -- | These Haskell types are injections to their synonyms in the
    -- APNS JSON spec. See
    -- <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CreatingtheNotificationPayload.html#//apple_ref/doc/uid/TP40008194-CH10-SW1
    -- here> and
    -- <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/PayloadKeyReference.html#//apple_ref/doc/uid/TP40008194-CH17-SW1
    -- here> for details.
    --
    -- These types are only sent to APNS (via SNS) and are never
    -- received on the wire, so they are not instances of "FromJSON",
    -- only 'ToJSON'.
    --
    -- Not all valid APNS fields are represented in these Haskell
    -- types; only the fields used by @pinpon@ are implemented.

    Alert(..)
  , title
  , body
  , defaultAlert

  , Aps(..)
  , alert
  , sound
  , defaultAps

  , Payload(..)
  , aps
  , defaultPayload
  ) where

import Protolude
import Control.Lens (makeLenses)
import Data.Aeson.Types
       (ToJSON(..), genericToEncoding, genericToJSON)
import Data.Text (Text)

import Network.PinPon.Util (recordTypeJSONOptions)

-- | A partial representation of the APNS @alert@ dictionary. See
-- <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/PayloadKeyReference.html#//apple_ref/doc/uid/TP40008194-CH17-SW4
-- here> for details.
--
-- Note that while the APNS wire protocol permits an @alert@ to be
-- represented as a single JSON string, rather than a dictionary, we
-- only support the dictionary representation.
data Alert = Alert
  { _title :: !Text
  , _body :: !Text
  } deriving (Show, Generic)

makeLenses ''Alert

-- | A @pinpon@-specific default value for 'Alert'.
defaultAlert :: Alert
defaultAlert = Alert "Ring! Ring!" "Someone is ringing the doorbell!"

instance ToJSON Alert where
  toJSON = genericToJSON recordTypeJSONOptions
  toEncoding = genericToEncoding recordTypeJSONOptions

-- | A partial representation of the APNS @aps@ dictionary. See
-- <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/PayloadKeyReference.html#//apple_ref/doc/uid/TP40008194-CH17-SW2
-- here> for details.
data Aps = Aps
  { _alert :: Alert
  , _sound :: !Text
  } deriving (Show, Generic)

makeLenses ''Aps

-- | A @pinpon@-specific default value for 'Aps'.
defaultAps :: Aps
defaultAps = Aps defaultAlert "default"

instance ToJSON Aps where
  toJSON = genericToJSON recordTypeJSONOptions
  toEncoding = genericToEncoding recordTypeJSONOptions

-- | The top-level APNS dictionary object. See
-- <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CreatingtheNotificationPayload.html#//apple_ref/doc/uid/TP40008194-CH10-SW15
-- here> for details.
--
-- Note that objects other than the @aps@ dictionary may be included
-- in APNS payloads, but as @pinpon@ doesn't use them, that
-- functionality is not implemented.
newtype Payload = Payload
  { _aps :: Aps
  } deriving (Show, Generic)

makeLenses ''Payload

-- | A @pinpon@-specific default value for an APNS 'Payload'.
defaultPayload :: Payload
defaultPayload = Payload defaultAps

instance ToJSON Payload where
  toJSON = genericToJSON recordTypeJSONOptions
  toEncoding = genericToEncoding recordTypeJSONOptions
