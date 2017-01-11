{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Catch.Pure (runCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Char8 as C8 (unpack)
import Data.Text (Text)
import Data.Time.Clock
       (NominalDiffTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status(..))
import Network.PinPon.Client (Notification(..), notify)
import Options.Applicative hiding (action)
import Options.Applicative.Text (text)
import Servant.Client (BaseUrl, ServantError(..), parseBaseUrl)
import System.GPIO.Linux.Sysfs (runSysfsGpioIO)
import System.GPIO.Monad
       (Pin(..), PinActiveLevel(..), PinInputMode(InputDefault),
        PinInterruptMode(..), withInterruptPin, pollInterruptPin)

-- Only one for now.
data Interpreter =
  SysfsIO
  deriving (Eq,Show,Read)

data Options =
  Options {_interpreter :: !Interpreter
          ,_edge :: !PinInterruptMode
          ,_activeLow :: !PinActiveLevel
          ,_pinNumber :: !Int
          ,_debounce :: !Int
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
  argument auto (metavar "INT" <>
                 value 5 <>
                 showDefault <>
                 help "Debounce duration in seconds")  <*>
  argument (str >>= parseServiceUrl)
           (metavar "URL" <>
            help "PinPon server base URL") <*>
  argument text (metavar "NAME" <>
                 help "Doorbell name (used to identify notification source)")

-- Note: debounce delay here is in /microseconds/.
debounce :: (MonadIO m) => Int -> m a -> m a
debounce delay action =
  do startAction <- liftIO getCurrentTime
     result <- action
     endAction <- liftIO getCurrentTime
     let timeLeft = max 0 $ delay - toUsec (endAction `diffUTCTime` startAction)
       in liftIO $ threadDelay timeLeft
     return result
  where
    toUsec :: NominalDiffTime -> Int
    toUsec d = truncate $ d * 1000000

-- Not really that pretty.
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

run :: Options -> IO ()
run (Options SysfsIO edge activeLevel pin debounceDelay serviceUrl name) =
  let notification = Notification name "Ring! Ring!"
  in
    do manager <- newManager tlsManagerSettings
       runSysfsGpioIO $
         withInterruptPin (Pin pin) InputDefault edge (Just activeLevel) $ \h ->
           forever $ debounce (debounceDelay * 1000000) $
             do void $ pollInterruptPin h
                liftIO $ runExceptT (notify notification manager serviceUrl) >>= \case
                  Right status -> print status
                  Left e -> putStrLn $ "PinPon service error: " ++ prettyServantError e

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info (helper <*> options)
           (fullDesc <>
            progDesc "pinpon-gpio" <>
            header "A GPIO-driven PinPon doorbell client.")

