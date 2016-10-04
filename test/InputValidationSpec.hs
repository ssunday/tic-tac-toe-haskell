module InputValidationSpec where

import Test.Hspec

import qualified InputValidation as SUT

spec :: Spec
spec = do
  describe "isYes" $ do
    it "is true when 'y'" $ do
      let result = SUT.isYes "y"
      result `shouldBe` True

    it "is true when 'yes'" $ do
      let result = SUT.isYes "yes"
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

    it "is true when 'yes'" $ do
      let result = SUT.isValidYOrNInput "yes"
      result `shouldBe` True

    it "is true when 'no'" $ do
      let result = SUT.isValidYOrNInput "no"
      result `shouldBe` True

    it "is false when 'z'" $ do
      let result = SUT.isValidYOrNInput "z"
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

  describe "isMenuOptionValid" $ do
    let menu =  [ (1, "Play Game")
                , (2, "Display Scores")
                , (3, "Quit") ]

    it "is true option is less than menu length" $ do
      let valid = SUT.isMenuOptionValid menu 2
      valid `shouldBe` True

    it "is true option is 1" $ do
      let valid = SUT.isMenuOptionValid menu 1
      valid `shouldBe` True

    it "is true option is max menu option" $ do
      let valid = SUT.isMenuOptionValid menu (length menu)
      valid `shouldBe` True

    it "is false when option is greater than length" $ do
      let valid = SUT.isMenuOptionValid menu 4
      valid `shouldBe` False

    it "is false when option is less than 1" $ do
      let valid = SUT.isMenuOptionValid menu 0
      valid `shouldBe` False
