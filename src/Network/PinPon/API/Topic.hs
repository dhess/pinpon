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

import Control.Lens ((^.), (&), (.~), (?~))
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Network.AWS.SNS.Publish
       (publish, pMessageStructure, pSubject, pTargetARN)
import Servant ((:>), JSON, Post, ReqBody, ServerT)
import Servant.HTML.Lucid (HTML)

import Network.PinPon.AWS (runSNS)
import Network.PinPon.Config (App(..), Config(..))
import Network.PinPon.Notification
       (Notification(..), headline, message)
import Network.PinPon.WireTypes.SNS
       (Message, defaultMessage, defaultText, apnsSandboxPayload)
import Network.PinPon.WireTypes.APNS
       (defaultPayload, aps, alert, body, title)
import Network.PinPon.Util (encodeText)

-- XXX dhess TODO: let the user specify which APNS payloads are
-- included.
toMessage :: Notification -> Message
toMessage n =
  let payload = defaultPayload
                  & aps.alert.title .~ n ^. headline
                  & aps.alert.body .~ n ^. message
  in
    defaultMessage
      & defaultText .~ n ^. message
      & apnsSandboxPayload ?~ payload

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
                                  & pSubject ?~ n ^. headline
                                  & pMessageStructure ?~ "json"
                                  & pTargetARN ?~ arn
         return n
