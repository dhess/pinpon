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
  ) where

import Control.Monad.Trans.Except (ExceptT)
import Data.Proxy (Proxy(..))
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ServantError, client)

import Network.PinPon.API (API)
import Network.PinPon.Notification
       (Notification(..), defaultNotification, headline, message)

-- | The client API.
clientAPI :: Proxy API
clientAPI = Proxy

-- | Post a notification.
notify :: Notification -> Manager -> BaseUrl -> ExceptT ServantError IO Notification
notify = client clientAPI
