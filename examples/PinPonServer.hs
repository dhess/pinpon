{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar)
import Control.Monad.Trans.AWS
       (Region(Oregon), Credentials(Discover), newEnv)
import qualified Data.Map.Strict as Map (empty)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import Network.PinPon.Model (AppDb(..))
import Network.PinPon.Config (Config(..))
import Network.PinPon.SwaggerAPI (app)

data Options = Options {_port :: !Int}

defaultConfig :: IO Config
defaultConfig =
  do env <- newEnv Oregon Discover
     db <- atomically $ newTVar AppDb {_topics = Map.empty}
     return Config {_awsEnv = env
                   ,_appDb = db }

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
