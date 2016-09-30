module SettingsSpec where

import Test.Hspec

import qualified Settings as SUT

main :: IO ()
main = hspec spec

spec :: Spec

spec = do
  describe "isMarkerValid" $ do
    it "is true when marker is of length one" $ do
      let marker = "X"
          valid = SUT.isMarkerValid marker
      valid `shouldBe` True

    it "is false when marker is greater than 1" $ do
      let marker = "rw"
          valid = SUT.isMarkerValid marker
      valid `shouldBe` False