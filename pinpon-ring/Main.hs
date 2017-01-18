{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.Catch.Pure (runCatch)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Char8 as C8 (unpack)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status(..))
import Network.PinPon.Client
       (Notification(..), defaultNotification, headline, message, notify,
        sound)
import Options.Applicative
import Options.Applicative.Text (text)
import Servant.Client (BaseUrl, ServantError(..), parseBaseUrl)
import System.Exit (ExitCode(..), exitSuccess, exitWith)

data Options = Options
  { _headline :: !Text
  , _message :: !Text
  , _sound :: !Text
  , _url :: !BaseUrl
  }

parseServiceUrl :: String -> ReadM BaseUrl
parseServiceUrl s =
  case runCatch $ parseBaseUrl s of
    Left _ -> readerError $ "Invalid service URL: " ++ s
    Right url -> return url

options :: Parser Options
options =
  Options <$>
  option text (long "headline" <>
               short 'H' <>
               metavar "TEXT" <>
               value (defaultNotification ^. headline) <>
               help "Override the default notification headline") <*>
  option text (long "message" <>
               short 'M' <>
               metavar "TEXT" <>
               value (defaultNotification ^. message) <>
               help "Override the default notification message") <*>
  option text (long "sound" <>
               short 'S' <>
               metavar "TEXT" <>
               value (defaultNotification ^. sound) <>
               help "Override the default notification sound") <*>
  argument (str >>= parseServiceUrl)
           (metavar "URL" <>
            help "PinPon server base URL")

run :: Options -> IO ()
run (Options hl msg s baseUrl) =
  let notification = Notification hl msg s
  in
    do manager <- newManager tlsManagerSettings
       runExceptT (notify notification manager baseUrl) >>= \case
         Right status ->
           do print status
              exitSuccess
         Left e ->
           do putStrLn $ "PinPon service error: " ++ prettyServantError e
              exitWith $ ExitFailure 1
  where
    prettyServantError :: ServantError -> String
    prettyServantError (FailureResponse status _ _) =
      show (statusCode status) ++ " " ++ C8.unpack (statusMessage status)
    prettyServantError DecodeFailure{} =
      "decode failure"
    prettyServantError UnsupportedContentType{} =
      "unsupported content type"
    prettyServantError InvalidContentTypeHeader{} =
      "invalid content type header"
    prettyServantError ConnectionError{} =
      "connection refused"

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
                (fullDesc <>
                 progDesc "Ring a PinPon doorbell" <>
                 header "pinpon-ring")
