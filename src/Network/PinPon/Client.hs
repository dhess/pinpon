{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.Client
  ( -- * Client actions
    notify

    -- * Re-exported for convenience.
  , Notification(..)
  , defaultNotification
  , headline
  , message
  , sound
  ) where

import Data.Proxy (Proxy(..))
import Servant.Client (ClientM, client)

import Network.PinPon.API (API)
import Network.PinPon.Notification
       (Notification(..), defaultNotification, headline, message, sound)

-- | The client API.
clientAPI :: Proxy API
clientAPI = Proxy

-- | Post a notification.
notify :: Notification -> ClientM Notification
notify = client clientAPI
