{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.PinPon.AWS
  ( -- * Types
    ServantSNSMonad

    -- * Functions
  , runSNS
  ) where

import Protolude hiding (catch)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.AWS (runAWST, send)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.ByteString.Lens (packedChars)
import Network.AWS (HasEnv(..))
import Network.AWS.Data.Text (ToText(..))
import Network.AWS.Types (AWSRequest, Error(..), Rs, serializeMessage, serviceMessage)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Servant.Server (ServerError(..), err502, err504)

type ServantSNSMonad m r a = (AWSRequest a, HasEnv r, MonadReader r m, MonadCatch m, MonadResource m)

runSNS :: (ServantSNSMonad m r a) => a -> m (Rs a)
runSNS req =
  do env <- ask
     catch (runAWST env $ send req) $ throwIO . snsErrToServant

snsErrToServant :: Error -> ServerError
snsErrToServant e = (errCode e) { errBody = mconcat ["Upstream AWS SNS error: ", errMsg e ] }

errCode :: Error -> ServerError
errCode (TransportError (HttpExceptionRequest _ ResponseTimeout)) = err504
errCode (TransportError (HttpExceptionRequest _ ConnectionTimeout)) = err504
errCode _ = err502

errMsg :: Error -> BL.ByteString
errMsg (ServiceError e) = maybe "Unspecified error" (toSL . toText) $ e ^. serviceMessage
errMsg (SerializeError e) = e ^. (serializeMessage . packedChars)
errMsg (TransportError e) = show e ^. packedChars
