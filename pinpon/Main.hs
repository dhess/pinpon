{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((&), (?~), (^.), view)
import Control.Monad.Trans.AWS
       (Region(Oregon), Credentials(Discover), newEnv, paginate,
        runResourceT, runAWST, send)
import Data.Conduit (($$), (=$=))
import qualified Data.Conduit.Combinators as Conduit (concatMap, find, sinkList)
import Data.Text (Text)
import Network.AWS (AWS)
import Network.AWS.SNS (Topic, createTopic, ctrsTopicARN, tTopicARN)
import Network.AWS.SNS.Publish (publish, pSubject, pTargetARN)

targetARN :: Text
targetARN = "arn:aws:sns:us-west-2:948017695415:test1"

main :: IO ()
main =
  do env <- newEnv Oregon Discover
     result <- runResourceT . runAWST env $
       do topic <- send $ createTopic "test1"
          r <- send $ publish "Yo"
                        & pSubject ?~ "Hi from Amazon SNS"
                        & pTargetARN ?~ targetARN
          return r
     print result
