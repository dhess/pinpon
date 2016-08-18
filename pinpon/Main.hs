{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.), view)
import Control.Monad.Trans.AWS
       (Region(Oregon), Credentials(Discover), newEnv, paginate,
        runResourceT, runAWST, send)
import Data.Conduit (($$), (=$=))
import qualified Data.Conduit.Combinators as Conduit (concatMap, find, sinkList)
import Data.Text (Text)
import Network.AWS (AWS)
import Network.AWS.SNS (Topic, createTopic, listTopics, ltrsTopics, tTopicARN)

targetARN :: Text
targetARN = "arn:aws:sns:us-west-2:948017695415:topic_name_test1"

topicMatch :: Text -> Topic -> Bool
topicMatch arn topic = maybe False (== arn) (view tTopicARN topic)

main :: IO ()
main =
  do env <- newEnv Oregon Discover
     result <- runResourceT . runAWST env $
       do topic <- send $ createTopic "test1"
          l <- paginate listTopics $$ Conduit.concatMap (view ltrsTopics) =$= Conduit.sinkList
          return l
     print result
