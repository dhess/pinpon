{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.Trans.AWS
       (Region(Oregon), Credentials(Discover), newEnv, runResourceT,
        runAWST, send)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network (PortID(..), listenOn)
import Network.AWS.SNS (createTopic, ctrsTopicARN)
import Network.PinPon.Config (Config(..))
import Network.PinPon.SwaggerAPI (app)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import Options.Applicative.Text (text)

data Options = Options {_port :: !Int
                       ,_topicName :: !Text}

defaultConfig :: Text -> IO Config
defaultConfig topicName =
  do env <- newEnv Oregon Discover
     topic <- runResourceT . runAWST env $
        send $ createTopic topicName
     return Config {_env = env
                   ,_arn = fromJust $ topic ^. ctrsTopicARN }

options :: Parser Options
options =
  Options <$>
  option auto (long "port" <>
               short 'p' <>
               metavar "INT" <>
               value 8000 <>
               help "Listen on port") <*>
  argument text (metavar "TOPIC_NAME")

run :: Options -> IO ()
run (Options port topicName) =
  do config <- defaultConfig $ topicName
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (app config)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
                ( fullDesc
                   <> progDesc "Run a PinPon server"
                   <> header "pinpon - A PinPon server" )
