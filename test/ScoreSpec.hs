module ScoreSpec where

import Test.Hspec

import qualified Score as SUT

spec :: Spec
spec = do
  describe "getTallys" $ do
    it "creates list of tuples of with the first element as the marker and second as occurrences of comma seperated list" $ do
      let markers = ["X", "Y", "X"]
          occurrences = SUT.tallyWinners markers
      occurrences `shouldBe` [("X", 2), ("Y", 1)]

    it "maintains order and uniqueness" $ do
      let markers = ["X", "Z", "Y", "W", "X", "Y", "P", "X"]
          occurrences = SUT.tallyWinners markers
      occurrences `shouldBe` [("X", 3), ("Z", 1), ("Y", 2), ("W", 1), ("P", 1)]
