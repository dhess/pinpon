{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import Network.PinPon.Server.API

data Options = Options {_port :: !Int}

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
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (app ())

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
                ( fullDesc
                   <> progDesc "Run a PinPon server"
                   <> header "pinpon-server - A PinPon server" )
