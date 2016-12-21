{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.API.Service
         ( -- * Types
           ServiceAPI

          -- * Servant functions
         , serviceServer
         ) where

import Servant
       ((:>), Get, JSON, ServerT)

import Network.PinPon.Model (Service(..), allServices)
import Network.PinPon.Types
       (App(..))

type ServiceAPI =
  "service" :> Get '[JSON] [Service]

serviceServer :: ServerT ServiceAPI App
serviceServer = return allServices
