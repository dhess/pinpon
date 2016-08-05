module Main where

import Control.Lens ((^.), view)
import Control.Monad.Trans.AWS
       (Region(Oregon), Credentials(Discover), newEnv, paginate,
        runResourceT, runAWST)
import Data.Conduit (($$), (=$=))
import qualified Data.Conduit.Combinators as Conduit (concatMap, sinkList)
import Network.AWS (AWS)
import Network.AWS.SNS (listTopics, ltrsTopics)

main :: IO ()
main =
  do env <- newEnv Oregon Discover
     result <- runResourceT . runAWST env $
       do l <- paginate listTopics $$ Conduit.concatMap (view ltrsTopics) =$= Conduit.sinkList
          return l
     print result
