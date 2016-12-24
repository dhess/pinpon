{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch.Pure (runCatch)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Char8 as C8 (unpack)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status(..))
import Network.PinPon.Client (Notification(..), notify)
import Options.Applicative
import Options.Applicative.Text (text)
import Servant.Client (BaseUrl, ServantError(..), parseBaseUrl)
import System.Exit (ExitCode(..), exitSuccess, exitWith)

data Options =
  Options {_url :: !BaseUrl
          ,_subject :: !Text
          ,_body :: !Text}

parseServiceUrl :: String -> ReadM BaseUrl
parseServiceUrl s =
  case runCatch $ parseBaseUrl s of
    Left _ -> readerError $ "Invalid service URL: " ++ s
    Right url -> return url

options :: Parser Options
options =
  Options <$>
  argument (str >>= parseServiceUrl)
           (metavar "URL" <>
            help "PinPon server base URL") <*>
  argument text (metavar "SUBJECT") <*>
  argument text (metavar "BODY")

run :: Options -> IO ()
run (Options baseUrl subject body) =
  let notification = Notification subject body
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
                 progDesc "'Ring' a PinPon doorbell" <>
                 header "pinpon-ring")
