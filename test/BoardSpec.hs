module BoardSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified SpecHelper as Helper

import Board as SUT

spec :: Spec
spec = do
  describe "makeBoard" $ do
    it "returns a map of a blank board" $ do
      SUT.makeBoard `shouldBe` Helper.constructBoard []

  describe "markBoard" $ do
    prop "marks board with given spot and marker" $ \marker ->
      let markedBoard = SUT.markBoard (Helper.constructBoard []) "2" marker
          markedVal = Helper.valueFromKey markedBoard "2"
      in markedVal == marker

  describe "getAvailableSpots" $ do
    it "returns all blank spot keys" $ do
      let board = Helper.constructBoard [ ("2", "X"), ("3", "O")
                                        , ("5", "O"), ("6", "X")
                                        , ("7", "X"), ("8", "O") ]
          availableSpots = SUT.getAvailableSpots board
      availableSpots `shouldBe` [ "1", "4", "9" ]

  describe "getBoardValues" $ do
    it "returns all board values for given spots" $ do
      let board = Helper.constructBoard [ ("2", "X"), ("3", "O")
                                        , ("5", "X"), ("6", "X")
                                        , ("7", "X"), ("8", "O")]
          boardVals = SUT.getBoardValues board ["1", "2", "3", "7", "8", "9"]
      boardVals `shouldBe` [ "", "X", "O"
                           , "X", "O", "" ]

  describe "isSpotOpen" $ do
    it "returns false when spot is not blank" $ do
      let board = Helper.constructBoard [ ("7", "X") ]
          isOpen = SUT.isSpotOpen board "7"
      isOpen `shouldBe` False

    it "returns false when spot is not on the board" $ do
      let board = Helper.constructBoard []
          isOpen = SUT.isSpotOpen board "120"
      isOpen `shouldBe` False

    it "returns true when spot is blank" $ do
      let board = Helper.constructBoard []
          isOpen = SUT.isSpotOpen board "1"
      isOpen `shouldBe` True
