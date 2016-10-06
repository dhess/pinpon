{-|
Module      : Network.PinPon.Server
Description : Top-level server re-exports
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

This module re-exports the @pinpon@ server modules.

-}

module Network.PinPon.Server
  (
   -- * The standard server API
   module Network.PinPon.Server.API
  )
  where

import Network.PinPon.Server.API
