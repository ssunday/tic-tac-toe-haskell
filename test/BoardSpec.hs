module BoardSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as Map (fromList, lookup)
import qualified Data.Maybe as Maybe (fromMaybe)

import Board as SUT

main :: IO ()
main = hspec spec

spec :: Spec

spec = do
  describe "makeBoard" $ do
    it "returns a map of a blank board" $ do
      SUT.makeBoard `shouldBe` Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                                            , ("4", ""), ("5", ""), ("6", "")
                                            , ("7", ""), ("8", ""), ("9", "")]

  describe "markBoard" $ do
    it "marks board with given spot and marker" $ property $ \x ->
      let markedBoard = SUT.markBoard SUT.makeBoard "2" x
          markedVal = Maybe.fromMaybe "" (Map.lookup "2" markedBoard)
      in markedVal == x

  describe "getAvailableSpots" $ do
    it "returns all blank spot keys" $ do
      let board = Map.fromList [ ("1", ""), ("2", "X"), ("3", "O")
                               , ("4", ""), ("5", "X"), ("6", "X")
                               , ("7", "X"), ("8", "O"), ("9", "")]
          availableSpots = SUT.getAvailableSpots board
      availableSpots `shouldBe` [ "1", "4", "9" ]

  describe "getBoardValues" $ do
    it "returns all board values for given spots" $ do
      let board = Map.fromList [ ("1", ""), ("2", "X"), ("3", "O")
                               , ("4", ""), ("5", "X"), ("6", "X")
                               , ("7", "X"), ("8", "O"), ("9", "")]
          boardVals = SUT.getBoardValues board ["1", "2", "3", "7", "8", "9"]
      boardVals `shouldBe` [ "", "X", "O",
                             "X", "O", "" ]

  describe "isSpotOpen" $ do
    it "returns false when spot is not blank" $ do
      let board = Map.fromList [ ("1", ""), ("2", "X"), ("3", "O")
                               , ("4", ""), ("5", "X"), ("6", "X")
                               , ("7", "X"), ("8", "O"), ("9", "")]
          isOpen = SUT.isSpotOpen board "7"
      isOpen `shouldBe` False

    it "returns false when spot is on the board" $ do
      let board = Map.fromList [ ("1", ""), ("2", "X"), ("3", "O")
                               , ("4", ""), ("5", "X"), ("6", "X")
                               , ("7", "X"), ("8", "O"), ("9", "")]
          isOpen = SUT.isSpotOpen board "120"
      isOpen `shouldBe` False

    it "returns true when spot is blank" $ do
      let board = Map.fromList [ ("1", ""), ("2", "X"), ("3", "O")
                               , ("4", ""), ("5", "X"), ("6", "X")
                               , ("7", "X"), ("8", "O"), ("9", "")]
          isOpen = SUT.isSpotOpen board "1"
      isOpen `shouldBe` True
