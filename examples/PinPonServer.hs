{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.AWS
       (Region(Oregon), Credentials(Discover), newEnv)
import qualified Data.Map.Strict as Map (fromList)
import Data.Text (Text)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import Network.PinPon.Config (Config(..), Service(..))
import Network.PinPon.Swagger (app)

data Options = Options {_port :: !Int}

targetARN :: Text
targetARN = "arn:aws:sns:us-west-2:948017695415:test1"

defaultConfig :: IO Config
defaultConfig =
  do env <- newEnv Oregon Discover
     return Config {_awsEnv = env
                   ,_keyToTopic = Map.fromList [("test1", AWS targetARN)]}

options :: Parser Options
options =
  Options <$>
  option auto (long "port" <>
               short 'p' <>
               metavar "INT" <>
               value 8000 <>
               help "Listen on port")

run :: Options -> IO ()
run (Options port) =
  do config <- defaultConfig
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (app config)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
                ( fullDesc
                   <> progDesc "Run a PinPon server"
                   <> header "pinpon-server - A PinPon server" )
