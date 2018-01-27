{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

import Protolude
import Control.Lens ((^.), (&), (.~), (?~))
import Control.Monad (void)
import Control.Monad.Reader (asks)
import qualified Data.Set as Set (member)
import Network.AWS.SNS.Publish
       (publish, pMessageStructure, pSubject, pTargetARN)
import Servant ((:>), JSON, Post, ReqBody, ServerT)
import Servant.HTML.Lucid (HTML)

import Network.PinPon.AWS (runSNS)
import Network.PinPon.Config
       (App(..), Config(..), Platform(..))
import Network.PinPon.Notification
       (Notification(..), headline)
import Network.PinPon.WireTypes.SNS
       (Message(..), apnsPayload, apnsSandboxPayload, defaultMessage,
        defaultText)
import Network.PinPon.WireTypes.APNS
       (defaultPayload, aps, alert, body, sound, title)
import Network.PinPon.Util (encodeText)

toMessage ::  Notification -> App Message
toMessage (Notification h m s) =
  let payload =
        defaultPayload & aps.alert.title .~ h & aps.alert.body .~ m & aps.sound .~ s
  in do
    platforms <- asks _platforms
    return $
      defaultMessage
        & defaultText .~ m
        & apnsPayload .~ (if Set.member APNS platforms then Just payload else Nothing)
        & apnsSandboxPayload .~ (if Set.member APNSSandbox platforms then Just payload else Nothing)

type TopicAPI =
  "topic" :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

topicServer :: ServerT TopicAPI App
topicServer =
  notify
  where
    notify :: Notification -> App Notification
    notify n =
      do arn <- asks _arn
         msg <- toMessage n
         void $ runSNS $ publish (encodeText msg)
                                  & pSubject ?~ n ^. headline
                                  & pMessageStructure ?~ "json"
                                  & pTargetARN ?~ arn
         return n
