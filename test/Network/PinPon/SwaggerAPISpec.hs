{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.PinPon.SwaggerAPISpec (spec) where

import Protolude

import Network.PinPon.API (api)
import Network.PinPon.API.Topic (Notification(..))
import Network.PinPon.SwaggerAPI (pinPonSwagger)

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as C8 (readFile)
import Paths_pinpon
import Servant.Swagger.Test
import Test.Hspec
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Instances ()

spec :: Spec
spec =
  do describe "Swagger" $
       do context "ToJSON matches ToSchema" $
            validateEveryToJSON api
          context "swagger.json" $
            it "matches current file contents" $
              do path <- getDataFileName "swagger.json"
                 swagger <- eitherDecode <$> C8.readFile path
                 swagger `shouldBe` Right pinPonSwagger

instance Arbitrary Notification where
  arbitrary = Notification <$> arbitrary <*> arbitrary <*> arbitrary
