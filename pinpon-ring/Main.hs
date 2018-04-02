{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude hiding (option)
import Control.Lens ((^.))
import Control.Monad.Catch.Pure (runCatch)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (Text, pack)
import qualified Data.Text as T (unwords)
import qualified Data.Text.IO as T (hPutStrLn)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.PinPon.Client
       (Notification(..), defaultNotification, headline, message, notify,
        sound)
import Options.Applicative
import Options.Applicative.Text (text)
import Servant.Client
  ( BaseUrl
  , ServantError(..)
  , mkClientEnv
  , parseBaseUrl
  , runClientM
  )
import Servant.Client.Core (GenResponse(responseBody, responseStatusCode))
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

-- Not really that pretty.
prettyServantError :: ServantError -> Text
prettyServantError (FailureResponse response) =
  T.unwords
    [pack (show $ responseStatusCode response), toS $ responseBody response]
prettyServantError DecodeFailure{} =
  "decode failure"
prettyServantError UnsupportedContentType{} =
  "unsupported content type"
prettyServantError InvalidContentTypeHeader{} =
  "invalid content type header"
prettyServantError ConnectionError{} =
  "connection error"

run :: Options -> IO ()
run (Options hl msg s baseUrl) =
  let notification = Notification hl msg s
  in
    do manager <- newManager tlsManagerSettings
       runClientM (notify notification) (mkClientEnv manager baseUrl) >>= \case
         Right status ->
           do print status
              exitSuccess
         Left e ->
           do outputErr $ T.unwords ["PinPon service error:", prettyServantError e]
              exitWith $ ExitFailure 1
  where
    outputErr :: Text -> IO ()
    outputErr = T.hPutStrLn stderr

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
                (fullDesc <>
                 progDesc "Ring a PinPon doorbell" <>
                 header "pinpon-ring")
