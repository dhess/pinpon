{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.Trans.AWS
       (Region(..), Credentials(Discover), newEnv, runResourceT, runAWST,
        send)

import Data.Maybe (fromJust)
import Data.Text (Text)
import Network (PortID(..), listenOn)
import Network.AWS.Data (fromText)
import Network.AWS.SNS (createTopic, ctrsTopicARN)
import Network.PinPon.Config (Config(..))
import Network.PinPon.SwaggerAPI (app)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import Options.Applicative.Text (text)

data Options = Options
  { _region :: !Region
  , _port :: !Int
  , _topicName :: !Text
  }

parseRegion :: Text -> ReadM Region
parseRegion r =
  case fromText r of
    Left _ -> readerError $ "Invalid AWS region: " ++ show r
    Right region -> return region

defaultConfig :: Region -> Text -> IO Config
defaultConfig region topicName =
  do env <- newEnv region Discover
     topic <- runResourceT . runAWST env $
        send $ createTopic topicName
     return Config {_env = env
                   ,_arn = fromJust $ topic ^. ctrsTopicARN }

options :: Parser Options
options =
  Options <$>
  option (text >>= parseRegion)
         (long "region" <>
          short 'r' <>
          metavar "REGION" <>
          value NorthVirginia <>
          help "Specify the AWS region (default is us-east-1)") <*>
  option auto (long "port" <>
               short 'p' <>
               metavar "INT" <>
               value 8000 <>
               help "Listen on port") <*>
  argument text (metavar "topic-name")

run :: Options -> IO ()
run (Options region port topicName) =
  do config <- defaultConfig region $ topicName
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (app config)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
                ( fullDesc
                   <> progDesc "Run a PinPon server"
                   <> header "pinpon - A PinPon server" )
