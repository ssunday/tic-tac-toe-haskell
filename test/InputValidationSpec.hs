module InputValidationSpec where

import Test.Hspec

import qualified InputValidation as SUT

spec :: Spec
spec = do
  describe "isYes" $ do
    it "is true when 'y'" $ do
      let result = SUT.isYes "y"
      result `shouldBe` True

    it "is false when 'n'" $ do
      let result = SUT.isYes "n"
      result `shouldBe` False

  describe "isValidYOrNInput" $ do
    it "is true when 'y'" $ do
      let result = SUT.isValidYOrNInput "y"
      result `shouldBe` True

    it "is true when 'n'" $ do
      let result = SUT.isValidYOrNInput "n"
      result `shouldBe` True

    it "is false when 'z'" $ do
      let result = SUT.isValidYOrNInput "z"
      result `shouldBe` False

    it "is false when 'yes'" $ do
      let result = SUT.isValidYOrNInput "yes"
      result `shouldBe` False

  describe "isMarkerValid" $ do
    it "is true when marker is of length one" $ do
      let marker = "X"
          valid = SUT.isMarkerValid marker
      valid `shouldBe` True

    it "is false when marker is greater than 1" $ do
      let marker = "rw"
          valid = SUT.isMarkerValid marker
      valid `shouldBe` False
