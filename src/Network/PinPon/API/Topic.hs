{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.API.Topic
         ( -- * Types
           Notification(..)
         , TopicAPI

           -- * Servant functions
         , topicServer
         ) where

import Control.Lens ((^.), (&), (?~))
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Network.AWS.SNS.Publish
       (publish, pMessageStructure, pSubject, pTargetARN)
import Servant ((:>), JSON, Post, ReqBody, ServerT)
import Servant.HTML.Lucid (HTML)

import Network.PinPon.AWS (runSNS)
import Network.PinPon.Config (App(..), Config(..))
import Network.PinPon.Notification (Notification(..), body, title)
import Network.PinPon.WireTypes.SNS (Message(..))
import Network.PinPon.WireTypes.APNS
       (Alert(..), Aps(..), Payload(..))
import Network.PinPon.Util (encodeText)

toMessage :: Notification -> Message
toMessage n =
  Message
  { _defaultMsg = n ^. body
  , _apnsPayload = Nothing
  , _apnsSandboxPayload =
    Just
      Payload
      { _aps =
        Aps
        { _alert =
          Alert
          { _title = n ^. title
          , _body = "Ring! Ring!"
          }
        , _sound = "default"
        }
      }
  }

type TopicAPI =
  "topic" :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

topicServer :: ServerT TopicAPI App
topicServer =
  notify
  where
    notify :: Notification -> App Notification
    notify n =
      do arn <- asks _arn
         void $ runSNS $ publish (encodeText $ toMessage n)
                                  & pSubject ?~ n ^. title
                                  & pMessageStructure ?~ "json"
                                  & pTargetARN ?~ arn
         return n
