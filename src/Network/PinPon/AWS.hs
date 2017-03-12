{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.PinPon.AWS
  ( runSNS
  ) where

import Control.Lens ((^.))
import Control.Monad.Catch (catch)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.AWS (runAWST, send)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.ByteString.Lens (packedChars)
import Network.AWS.Data.ByteString (ToByteString(..))
import Network.AWS.Data.Text (ToText(..))
import Network.AWS.Types (AWSRequest, Error(..), Rs, serializeMessage, serviceMessage)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Servant (ServantErr(..), err502, err504, throwError)

import Network.PinPon.Config (App(..), Config(..))

runSNS :: (AWSRequest a) => a -> App (Rs a)
runSNS req =
  do env <- asks _env
     catch (runAWST env $ send req) $ throwError . snsErrToServant

snsErrToServant :: Error -> ServantErr
snsErrToServant e = (errCode e) { errBody = mconcat ["Upstream AWS SNS error: ", errMsg e ] }

errCode :: Error -> ServantErr
errCode (TransportError (HttpExceptionRequest _ ResponseTimeout)) = err504
errCode (TransportError (HttpExceptionRequest _ ConnectionTimeout)) = err504
errCode _ = err502

errMsg :: Error -> BL.ByteString
errMsg (ServiceError e) = maybe "Unspecified error" (fromStrict . toBS . toText) $ e ^. serviceMessage
errMsg (SerializeError e) = e ^. serializeMessage ^. packedChars
errMsg (TransportError e) = show e ^. packedChars
