{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Options.Applicative
import System.GPIO.Linux.Sysfs (runSysfsGpioIO)
import System.GPIO.Monad
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

-- Only one for now.
data Interpreter =
  SysfsIO
  deriving (Eq,Show,Read)

data Options =
  Options {_interpreter :: !Interpreter
          ,_edge :: !PinInterruptMode
          ,_activeLow :: !PinActiveLevel
          ,_pinNumber :: !Int
          ,_url :: !BaseUrl
          ,_name :: !Text}

parseServiceUrl :: String -> ReadM BaseUrl
parseServiceUrl s =
  case runCatch $ parseBaseUrl s of
    Left _ -> readerError $ "Invalid service URL: " ++ s
    Right url -> return url

options :: Parser Options
options =
  Options <$>
  option auto (long "interpreter" <>
               short 'i' <>
               metavar "SysfsIO" <>
               value SysfsIO <>
               showDefault <>
               help "Choose the GPIO interpreter to use") <*>
  option auto (long "edge" <>
               short 'e' <>
               metavar "RisingEdge|FallingEdge" <>
               value RisingEdge <>
               showDefault <>
               help "Trigger on rising/falling edge") <*>
  option auto (long "active-level" <>
               short 'l' <>
               metavar "ActiveLow|ActiveHigh" <>
               value ActiveHigh <>
               showDefault <>
               help "Pin active level") <*>
  argument auto (metavar "N" <>
                 help "GPIO pin number")  <*>
  argument (str >>= parseServiceUrl)
           (metavar "URL" <>
            help "PinPon server base URL") <*>
  argument text (metavar "NAME" <>
                 help "Doorbell name (used to identify notification source)")

run :: Options -> IO ()
run (Options SysfsIO edge activeLevel pin serviceUrl name) =
  runSysfsGpioIO $
    withInterruptPin (Pin pin) InputDefault edge (Just activeLevel) $ \h ->
      forever $
        do void $ pollInterruptPin h
           liftIO $ ring serviceUrl name

ring :: BaseUrl -> Text -> IO ()
ring url name =
  let notification = Notification name "Ring! Ring!"
  in
    do manager <- newManager tlsManagerSettings
       runExceptT (notify notification manager url) >>= \case
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

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info (helper <*> options)
           (fullDesc <>
            progDesc "pinpon-gpio" <>
            header "A GPIO-driven PinPon doorbell client.")

