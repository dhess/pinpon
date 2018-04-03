{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.PinPon.API.Topic
         ( -- * Types
           Notification(..)
         , TopicAPI
         , TopicMonad

           -- * Servant functions
         , topicServer
         ) where

import Protolude
import Control.Lens ((^.), (&), (.~), (?~))
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Set as Set (member)
import Network.AWS (HasEnv(..))
import Network.AWS.SNS.Publish
       (publish, pMessageStructure, pSubject, pTargetARN)
import Servant ((:>), JSON, Post, ReqBody, ServerT)
import Servant.HTML.Lucid (HTML)

import Network.PinPon.AWS (runSNS)
import Network.PinPon.Config (HasConfig(..), Platform(..))
import Network.PinPon.Notification
       (Notification(..), headline)
import Network.PinPon.WireTypes.SNS
       (Message(..), apnsPayload, apnsSandboxPayload, defaultMessage,
        defaultText)
import Network.PinPon.WireTypes.APNS
       (defaultPayload, aps, alert, body, sound, title)
import Network.PinPon.Util (encodeText)

type MessageMonad m r = (HasConfig r, MonadCatch m, MonadResource m, MonadReader r m)
type TopicMonad m r = (MessageMonad m r, HasEnv r)

toMessage :: (MessageMonad m r) => Notification -> m Message
toMessage (Notification h m s) =
  let payload =
        defaultPayload & aps.alert.title .~ h & aps.alert.body .~ m & aps.sound .~ s
  in do
    rdr <- ask
    let plts = rdr ^. platforms
    return $
      defaultMessage
        & defaultText .~ m
        & apnsPayload .~ (if Set.member APNS plts then Just payload else Nothing)
        & apnsSandboxPayload .~ (if Set.member APNSSandbox plts then Just payload else Nothing)

type TopicAPI =
  "topic" :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

topicServer :: (TopicMonad m r) => ServerT TopicAPI m
topicServer =
  notify
  where
    notify :: (TopicMonad m r) => Notification -> m Notification
    notify n =
      do rdr <- ask
         let theArn = rdr ^. arn
         msg <- toMessage n
         void $ runSNS $ publish (encodeText msg)
                                  & pSubject ?~ n ^. headline
                                  & pMessageStructure ?~ "json"
                                  & pTargetARN ?~ theArn
         return n
