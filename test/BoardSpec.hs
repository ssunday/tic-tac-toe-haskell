module BoardSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified Data.List as List

import qualified SpecHelper as Helper

import Board as SUT

spec :: Spec
spec = do
  describe "makeBoard" $ do
    it "returns a map of a blank 3x3 board when 3 is passed in" $ do
      SUT.makeBoard 3 `shouldBe` Helper.constructBoard 3 []

    it "returns a map of a blank 4x4 board when 4 is passed in" $ do
      SUT.makeBoard 4 `shouldBe` Helper.constructBoard 4 []

  describe "boardDimension" $ do
    it "returns 3 when 3x3 board passed in" $ do
      let board = Helper.constructBoard 3 []
      SUT.boardDimension board `shouldBe`3

    it "returns 4 when 4x4 board passed in" $ do
      let board = Helper.constructBoard 4 []
      SUT.boardDimension board `shouldBe` 4

  describe "markBoard" $ do
    prop "marks 3x3 board with given spot and marker" $ \marker ->
      let markedBoard = SUT.markBoard (Helper.constructBoard 3 []) "2" marker
          markedVal = Helper.valueFromKey markedBoard "2"
      in markedVal == marker

    prop "marks 4x4 board with given spot and marker" $ \marker ->
      let markedBoard = SUT.markBoard (Helper.constructBoard 4 []) "15" marker
          markedVal = Helper.valueFromKey markedBoard "15"
      in markedVal == marker

  describe "getAvailableSpots" $ do
    it "returns all blank spot keys for 3x3 board" $ do
      let board = Helper.constructBoard 3 [ ("2", "X"), ("3", "O")
                                          , ("5", "O"), ("6", "X")
                                          , ("7", "X"), ("8", "O") ]
          availableSpots = SUT.getAvailableSpots board
      availableSpots `shouldBe` [ "1", "4", "9" ]

    it "returns all blank spot keys for 4x4 board" $ do
      let board = Helper.constructBoard 4 [ ("2", "X"), ("3", "O")
                                          , ("5", "O"), ("6", "X")
                                          , ("7", "X"), ("8", "O")
                                          , ("15", "X"), ("16", "O")]
          availableSpots = SUT.getAvailableSpots board
      availableSpots `shouldBe` (List.sort [ "1", "4", "9", "10", "11", "12", "13", "14" ])

  describe "getBoardValues" $ do
    it "returns all board values for given spots" $ do
      let board = Helper.constructBoard 3 [ ("2", "X"), ("3", "O")
                                          , ("5", "X"), ("6", "X")
                                          , ("7", "X"), ("8", "O")]
          boardVals = SUT.getBoardValues board ["1", "2", "3", "7", "8", "9"]
      boardVals `shouldBe` [ "", "X", "O"
                           , "X", "O", "" ]

  describe "isSpotOpen" $ do
    it "returns false when spot is not blank" $ do
      let board = Helper.constructBoard 3 [ ("7", "X") ]
          isOpen = SUT.isSpotOpen board "7"
      isOpen `shouldBe` False

    it "returns false when spot is not on 3x3 board" $ do
      let board = Helper.constructBoard 3 []
          isOpen = SUT.isSpotOpen board "12"
      isOpen `shouldBe` False

    it "returns false when spot is not on 4x4 board" $ do
      let board = Helper.constructBoard 4 []
          isOpen = SUT.isSpotOpen board "20"
      isOpen `shouldBe` False

    it "returns true when spot is blank" $ do
      let board = Helper.constructBoard 3 []
          isOpen = SUT.isSpotOpen board "1"
      isOpen `shouldBe` True
