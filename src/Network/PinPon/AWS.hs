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
import Network.AWS.Types (Error(..), serializeMessage, serviceMessage)
import Network.AWS.SNS.Publish (Publish, PublishResponse)
import Network.HTTP.Client (HttpException(..))
import Servant (ServantErr(..), err502, err504, throwError)

import Network.PinPon.Config (App(..), Config(..))

runSNS :: Publish -> App PublishResponse
runSNS publish =
  do env <- asks _awsEnv
     catch (runAWST env $ send publish) $ throwError . snsErrToServant

snsErrToServant :: Error -> ServantErr
snsErrToServant e = (errCode e) { errBody = mconcat ["Upstream AWS SNS error: ", errMsg e ] }

errCode :: Error -> ServantErr
errCode (TransportError ResponseTimeout) = err504
errCode (TransportError (FailedConnectionException _ _)) = err504 -- See http-client docs.
errCode _ = err502

errMsg :: Error -> BL.ByteString
errMsg (ServiceError e) = maybe "Unspecified error" (fromStrict . toBS . toText) $ e ^. serviceMessage
errMsg (SerializeError e) = e ^. serializeMessage ^. packedChars
errMsg (TransportError e) = show e ^. packedChars
