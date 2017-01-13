{-# LANGUAGE DataKinds #-}
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

import Control.Lens ((&), (?~))
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Network.AWS.SNS.Publish (publish, pSubject, pTargetARN)
import Servant ((:>), JSON, Post, ReqBody, ServerT)
import Servant.HTML.Lucid (HTML)

import Network.PinPon.AWS (runSNS)
import Network.PinPon.Config (App(..), Config(..))
import Network.PinPon.Notification (Notification(..))

type TopicAPI =
  "topic" :> ReqBody '[JSON] Notification :> Post '[JSON, HTML] Notification

topicServer :: ServerT TopicAPI App
topicServer =
  notify
  where
    notify :: Notification -> App Notification
    notify n =
      do arn <- asks _arn
         void $ runSNS $ publish (_body n)
                                  & pSubject ?~ _title n
                                  & pTargetARN ?~ arn
         return n
