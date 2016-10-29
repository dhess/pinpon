{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.PinPon.SwaggerSpec (spec) where

import Network.PinPon.API (pinPonAPI)
import Network.PinPon.API.Notify (Notification(..))
import Network.PinPon.Config (Service(..))
import Network.PinPon.Swagger (pinPonSwagger)

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as C8 (readFile)
import Paths_pinpon
import Servant.Swagger.Test
import Test.Hspec
import Test.QuickCheck (Arbitrary(..), oneof, property)
import Test.QuickCheck.Instances ()

spec :: Spec
spec =
  do describe "Swagger" $
       do context "ToJSON matches ToSchema" $
            validateEveryToJSON pinPonAPI
          context "swagger.json" $
            it "matches current file contents" $
              do path <- getDataFileName "swagger.json"
                 swagger <- eitherDecode <$> C8.readFile path
                 swagger `shouldBe` Right pinPonSwagger

instance Arbitrary Service where
  arbitrary = oneof [AWS <$> arbitrary]

instance Arbitrary Notification where
  arbitrary = Notification <$> arbitrary <*> arbitrary

