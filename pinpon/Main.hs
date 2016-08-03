module Main where

import Network.PinPon.Actions (greet, sup, ciao, goodbye)
import Options.Applicative

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Hello HelloOptions
  | Sup String
  | Ciao
  | GoodBye GoodByeOptions

data HelloOptions =
  HelloOptions {greeting :: String
               ,target :: String}

data GoodByeOptions =
  GoodByeOptions {signoff :: String
                 ,goodbyeTarget :: String}

helloCmd :: Parser Command
helloCmd = Hello <$> helloOptions

helloOptions :: Parser HelloOptions
helloOptions =
  HelloOptions <$>
  strOption (long "greeting" <>
             short 'g' <>
             value "Hello" <>
             metavar "GREETING" <>
             help "The greeting") <*>
  argument str (metavar "TARGET")

supCmd :: Parser Command
supCmd =
  Sup <$>
  (argument str (metavar "TARGET"))

goodByeCmd :: Parser Command
goodByeCmd = GoodBye <$> goodByeOptions

goodByeOptions :: Parser GoodByeOptions
goodByeOptions =
  GoodByeOptions <$>
  strOption (long "signoff" <>
             short 's' <>
             value "Bye" <>
             metavar "SIGNOFF" <>
             help "The signoff") <*>
  argument str (metavar "TARGET")

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  switch (long "quiet" <>
          short 'q' <>
          help "Be quiet") <*>
  flag Normal
       Verbose
       (long "verbose" <>
        short 'v' <>
        help "Enable verbose mode") <*>
  hsubparser
    (command "hello" (info helloCmd (progDesc "Say hello")) <>
     command "sup" (info supCmd (progDesc "Informal greeting")) <>
     command "ciao"
             (info (pure Ciao)
                   (progDesc "Informal goodbye")) <>
     command "goodbye" (info goodByeCmd (progDesc "Say goodbye")))

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ (Hello (HelloOptions g t))) = greet g t
run (GlobalOptions False _ (Sup t)) = sup t
run (GlobalOptions False _ Ciao) = ciao
run (GlobalOptions False _ (GoodBye (GoodByeOptions s t))) = goodbye s t
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Say hello and goodbye" <>
                header "pinpon-cmd - a command-based CLI for pinpon")
