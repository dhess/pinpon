 {-# LANGUAGE OverloadedStrings #-}

 module Main where

 import            Control.Lens
 import            Control.Monad
 import            Control.Monad.Catch (throwM)
 import            Control.Monad.IO.Class
 import            Control.Monad.Trans.AWS
 import            Data.Text                (Text)
 import qualified  Data.Text                as Text
 import qualified  Data.Text.IO             as Text
 import            Data.Time.Clock.POSIX
 import            Network.AWS (AWS)
 import            Network.AWS.EC2
 import            System.IO

 main :: IO ()
 main = do
     hSetBuffering stdout LineBuffering

     ts  <- Text.pack . show <$> getTimestamp
     env <- newEnv Oregon Discover
     r   <- runResourceT . runAWST env $ do
         say "Create KeyPair " ts
         k <- send (createKeyPair ts)

         let key    = Text.unpack ts ++ ".pem"
             trusty = "ami-d732f0b7" -- hvm:ebs-ssd for us-west-2

         say "Writing KeyPair material to " key
         liftIO (Text.writeFile key (k ^. ckprsKeyMaterial))

         say "Create SecurityGroup " ts
         g <- view csgrsGroupId <$>
             send (createSecurityGroup ts "amazonka-examples")

         say "Authorizing SSH on SecurityGroup " g
         void . send $ authorizeSecurityGroupIngress
             & asgiGroupId    ?~ g
             & asgiIPProtocol ?~ "tcp"
             & asgiFromPort   ?~ 22
             & asgiToPort     ?~ 22
             & asgiCIdRIP     ?~ "0.0.0.0/0"

         say "Launching Instance with ImageId " trusty
         i <- trying _Error $ send $ runInstances trusty 1 1
             & rKeyName          ?~ ts
             & rInstanceType     ?~ T2_Micro
             & rSecurityGroupIds .~ [g]

         either (\e -> do
                    say "Failed to Launch Instance " e
                    say "Deleting SecurityGroup " g
                    void . send $ deleteSecurityGroup & dsgGroupId ?~ g
                    say "Deleting KeyPair " ts
                    void . send $ deleteKeyPair ts
                    throwM e)
                return
                i

     print r

 getTimestamp :: IO Integer
 getTimestamp = truncate <$> getPOSIXTime

 say :: Show a => Text -> a -> AWS ()
 say msg = liftIO . Text.putStrLn . mappend msg . Text.pack . show
