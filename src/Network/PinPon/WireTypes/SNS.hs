{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PinPon.WireTypes.SNS
  ( -- * Amazon Simple Notification Service on-the-wire types
    Message(..)
  , defaultText
  , apnsPayload
  , apnsSandboxPayload
  , defaultMessage
  ) where

import Protolude
import Control.Lens (makeLenses)
import Data.Aeson.Types
       ((.=), Pair, Series, ToJSON(..), object, pairs)
import Data.Monoid ((<>))
import Data.Text (Text)

import Network.PinPon.Util (encodeText)
import qualified Network.PinPon.WireTypes.APNS as APNS (Payload(..))

-- $setup
-- >>> :set -XOverloadedStrings

-- | A multi-platform SNS message.
data Message = Message
  { _defaultText        :: !Text              -- ^ The default message contents (required by SNS)
  , _apnsPayload        :: Maybe APNS.Payload -- ^ Optional production APNS payload
  , _apnsSandboxPayload :: Maybe APNS.Payload -- ^ Optional sandbox APNS payload
  } deriving (Show, Generic)

makeLenses ''Message

-- | A @pinpon@-specific default value for 'Message'.
defaultMessage :: Message
defaultMessage = Message "Someone is ringing the doorbell!" Nothing Nothing

instance ToJSON Message where
  toJSON (Message d p s) = object $! ["default" .= d] <> apns p <> apnsSandbox s
    where
      apns :: Maybe APNS.Payload -> [Pair]
      apns Nothing = mempty
      apns v = ["APNS" .= encodeText v]
      apnsSandbox :: Maybe APNS.Payload -> [Pair]
      apnsSandbox Nothing = mempty
      apnsSandbox v = ["APNS_SANDBOX" .= encodeText v]
  toEncoding (Message d p s) = pairs $! "default" .= d <> apns p <> apnsSandbox s
    where
      apns :: Maybe APNS.Payload -> Series
      apns Nothing = mempty
      apns v = "APNS" .= encodeText v
      apnsSandbox :: Maybe APNS.Payload -> Series
      apnsSandbox Nothing = mempty
      apnsSandbox v = "APNS_SANDBOX" .= encodeText v

-- $
-- >>> import Network.PinPon.WireTypes.APNS (Alert(..), Aps(..), Payload(..))
-- >>> import Data.Aeson (encode)
-- >>> let alert1 = Alert "This is a production alert title" "This is a production alert body"
-- >>> let aps1 = Aps alert1 "production default"
-- >>> let payload1 = Payload aps1
-- >>> let msg1 = Message "This is the default message" (Just payload1) Nothing
-- >>> let encodedMsg1 = encode $ toJSON msg1
-- >>> encodedMsg1 == "{\"default\":\"This is the default message\",\"APNS\":\"{\\\"aps\\\":{\\\"alert\\\":{\\\"title\\\":\\\"This is a production alert title\\\",\\\"body\\\":\\\"This is a production alert body\\\"},\\\"sound\\\":\\\"production default\\\"}}\"}" || encodedMsg1 == "{\"APNS\":\"{\\\"aps\\\":{\\\"alert\\\":{\\\"title\\\":\\\"This is a production alert title\\\",\\\"body\\\":\\\"This is a production alert body\\\"},\\\"sound\\\":\\\"production default\\\"}}\",\"default\":\"This is the default message\"}"
-- True
-- >>> let alert2 = Alert "This is a sandbox alert title" "This is a sandbox alert body"
-- >>> let aps2 = Aps alert2 "sandbox default"
-- >>> let payload2 = Payload aps2
-- >>> let msg2 = Message "This is the default message" Nothing (Just payload2)
-- >>> let encodedMsg2 = encode $ toJSON msg2
-- >>> encodedMsg2 == "{\"default\":\"This is the default message\",\"APNS_SANDBOX\":\"{\\\"aps\\\":{\\\"alert\\\":{\\\"title\\\":\\\"This is a sandbox alert title\\\",\\\"body\\\":\\\"This is a sandbox alert body\\\"},\\\"sound\\\":\\\"sandbox default\\\"}}\"}" || encodedMsg2 == "{\"default\":\"This is the default message\",\"APNS_SANDBOX\":\"{\\\"aps\\\":{\\\"alert\\\":{\\\"title\\\":\\\"This is a sandbox alert title\\\",\\\"body\\\":\\\"This is a sandbox alert body\\\"},\\\"sound\\\":\\\"sandbox default\\\"}}\"}"
-- True
-- >>> let msg3 = Message "This is the default message" (Just payload1) (Just payload2)
-- >>> let encodedMsg3 = encode $ toJSON msg3
-- >>> encodedMsg3 == "{\"default\":\"This is the default message\",\"APNS_SANDBOX\":\"{\\\"aps\\\":{\\\"alert\\\":{\\\"title\\\":\\\"This is a sandbox alert title\\\",\\\"body\\\":\\\"This is a sandbox alert body\\\"},\\\"sound\\\":\\\"sandbox default\\\"}}\",\"APNS\":\"{\\\"aps\\\":{\\\"alert\\\":{\\\"title\\\":\\\"This is a production alert title\\\",\\\"body\\\":\\\"This is a production alert body\\\"},\\\"sound\\\":\\\"production default\\\"}}\"}" || encodedMsg3 == "{\"default\":\"This is the default message\",\"APNS_SANDBOX\":\"{\\\"aps\\\":{\\\"alert\\\":{\\\"title\\\":\\\"This is a sandbox alert title\\\",\\\"body\\\":\\\"This is a sandbox alert body\\\"},\\\"sound\\\":\\\"sandbox default\\\"}}\",\"APNS\":\"{\\\"aps\\\":{\\\"alert\\\":{\\\"title\\\":\\\"This is a production alert title\\\",\\\"body\\\":\\\"This is a production alert body\\\"},\\\"sound\\\":\\\"production default\\\"}}\"}"
-- True
