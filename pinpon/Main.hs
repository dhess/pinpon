{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.Trans.AWS
       (Region(..), Credentials(..), newEnv, runResourceT, runAWST, send)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network (PortID(..), listenOn)
import Network.AWS.Auth (credProfile, credFile)
import Network.AWS.Data (fromText)
import Network.AWS.SNS (createTopic, ctrsTopicARN)
import Network.PinPon.Config (Config(..))
import Network.PinPon.SwaggerAPI (app)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import Options.Applicative.Text (text)

data Options = Options
  { _region :: !Region
  , _credentialsFile :: Maybe FilePath
  , _credentialsProfile :: !Text
  , _port :: !Int
  , _topicName :: !Text
  }

parseRegion :: Text -> ReadM Region
parseRegion r =
  case fromText r of
    Left _ -> readerError $ "Invalid AWS region: " ++ show r
    Right region -> return region

makeCredentials :: Maybe FilePath -> Text -> IO Credentials
makeCredentials Nothing profile =
  do defaultPath <- credFile
     return $ FromFile profile defaultPath
makeCredentials (Just path) profile =
  return $ FromFile profile path

defaultConfig :: Region -> Credentials -> Text -> IO Config
defaultConfig region credentials topicName =
  do env <- newEnv region credentials
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
  optional (strOption (long "credentials-file" <>
                       short 'f' <>
                       metavar "PATH" <>
                       help "Path to AWS credentials file")) <*>
  option text (long "profile" <>
               short 'P' <>
               metavar "PROFILE_NAME" <>
               value credProfile <>
               help "Credentials profile name") <*>
  option auto (long "port" <>
               short 'p' <>
               metavar "INT" <>
               value 8000 <>
               help "Listen on port") <*>
  argument text (metavar "topic-name")

run :: Options -> IO ()
run (Options region maybeFile profile port topicName) =
  do credentials <- makeCredentials maybeFile profile
     config <- defaultConfig region credentials topicName
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (app config)


main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
                ( fullDesc
                   <> progDesc "Run a PinPon server"
                   <> header "pinpon - A PinPon server" )
