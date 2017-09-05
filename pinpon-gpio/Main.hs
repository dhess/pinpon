{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Control.Monad (forever, unless, void)
import Control.Monad.Catch.Pure (runCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Char8 as C8 (unpack)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as T (unwords)
import qualified Data.Text.IO as T (putStrLn, hPutStrLn)
import Data.Time.Clock
       (NominalDiffTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status(..))
import Network.PinPon.Client
       (Notification(..), defaultNotification, headline, message, notify,
        sound)
import Options.Applicative hiding (action)
import Options.Applicative.Text (text)
import Servant.Client
       (BaseUrl, ClientEnv(..), ServantError(..), parseBaseUrl,
        runClientM)
import System.GPIO.Linux.Sysfs (SysfsGpioIO, runSysfsGpioIO)
import System.GPIO.Monad
       (Pin(..), PinActiveLevel(..), PinInputMode(InputDefault),
        PinInterruptMode(..), withInterruptPin, pollInterruptPin)
import System.IO (stderr)

-- Only one for now.
data Interpreter =
  SysfsIO
  deriving (Eq,Show,Read)

data Options = Options
  { _quiet :: !Bool
  , _interpreter :: !Interpreter
  , _edge :: !PinInterruptMode
  , _activeLow :: !PinActiveLevel
  , _debounce :: !Int
  , _headline :: !Text
  , _message :: !Text
  , _sound :: !Text
  , _pinNumber :: !Int
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
  switch (long "quiet" <>
              short 'q' <>
              showDefault <>
              help "Only show errors") <*>
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
  option auto (long "debounce" <>
               short 'D' <>
               metavar "INT" <>
               value 5 <>
               showDefault <>
               help "Debounce duration in seconds")  <*>
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
  argument auto (metavar "N" <>
                 help "GPIO pin number")  <*>
  argument (str >>= parseServiceUrl)
           (metavar "URL" <>
            help "PinPon server base URL")

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
prettyServantError :: ServantError -> Text
prettyServantError (FailureResponse _ status _ _) =
  T.unwords
    [pack (show $ statusCode status), pack (C8.unpack $ statusMessage status)]
prettyServantError DecodeFailure{} =
  "decode failure"
prettyServantError UnsupportedContentType{} =
  "unsupported content type"
prettyServantError InvalidContentTypeHeader{} =
  "invalid content type header"
prettyServantError ConnectionError{} =
  "connection refused"

run :: Options -> IO ()
run (Options quiet SysfsIO edge activeLevel debounceDelay hl msg s pin serviceUrl) =
  let notification = Notification hl msg s
  in do manager <- newManager tlsManagerSettings
        let clientEnv = ClientEnv manager serviceUrl
        runSysfsGpioIO $
          withInterruptPin (Pin pin) InputDefault edge (Just activeLevel) $ \h ->
          forever $ debounce (debounceDelay * 1000000) $ do
            void $ pollInterruptPin h
            output "Ring! Ring!"
            result <- sendNotification notification clientEnv
            case result of
              Right _ -> output "Notification sent"
              Left e -> outputErr $ T.unwords ["PinPon service error:", prettyServantError e]
  where
    sendNotification :: Notification -> ClientEnv -> SysfsGpioIO (Either ServantError Notification)
    sendNotification n env = liftIO $ runClientM (notify n) env

    output :: Text -> SysfsGpioIO ()
    output t = unless quiet $ liftIO (T.putStrLn t)

    outputErr :: Text -> SysfsGpioIO ()
    outputErr = liftIO . T.hPutStrLn stderr

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info (helper <*> options)
           (fullDesc <>
            progDesc "pinpon-gpio" <>
            header "A GPIO-driven PinPon doorbell client.")

