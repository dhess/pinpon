{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map (fromList)
import Data.Text (Text)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import Network.PinPon.App (Config(..))
import Network.PinPon.Server.API

data Options = Options {_port :: !Int}

targetARN :: Text
targetARN = "arn:aws:sns:us-west-2:948017695415:test1"

defaultConfig :: Config
defaultConfig =
  Config { _keyToTopic = Map.fromList [("test1", targetARN)] }

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
  do sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (app defaultConfig)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
                ( fullDesc
                   <> progDesc "Run a PinPon server"
                   <> header "pinpon-server - A PinPon server" )
