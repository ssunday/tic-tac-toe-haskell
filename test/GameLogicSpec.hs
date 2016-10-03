module GameLogicSpec where

import Test.Hspec

import qualified SpecHelper as Helper

import GameLogic as SUT

spec :: Spec
spec = do
  describe "isOver" $ do
    it "returns false when game is not over" $ do
      let board = Helper.constructBoard []
          isGameOver = SUT.isOver board
      isGameOver `shouldBe` False

    it "returns true when game has been won" $ do
      let board = Helper.constructBoard [ ("1", "X")
                                        , ("4", "O"), ("5", "X")
                                        , ("8", "O"), ("9", "X")]
          isGameOver = SUT.isOver board
      isGameOver `shouldBe` True

    it "returns true when game has been tied" $ do
      let board = Helper.constructBoard [ ("1", "X"), ("2", "X"), ("3", "O")
                                        , ("4", "O"), ("5", "X"), ("6", "X")
                                        , ("7", "X"), ("8", "O"), ("9", "O")]
          isGameOver = SUT.isOver board
      isGameOver `shouldBe` True

  describe "isTied" $ do
    it "returns false when board is not tied" $ do
      let board = Helper.constructBoard [ ("1", "X"), ("2", "O")
                                        , ("4", "O")
                                        , ("8", "X")]
          gameTied = SUT.isTied board
      gameTied `shouldBe` False

    it "returns false when board is won" $ do
      let board = Helper.constructBoard [ ("1", "X"), ("2", "O"), ("3", "X")
                                        , ("4", "O"), ("5", "X")
                                        , ("7", "X"), ("8", "X")]
          gameTied = SUT.isTied board
      gameTied `shouldBe` False

    it "returns true when board is tied" $ do
      let board = Helper.constructBoard [ ("1", "X"), ("2", "O"), ("3", "X")
                                        , ("4", "O"), ("5", "X"), ("6", "O")
                                        , ("7", "O"), ("8", "X"), ("9", "O")]
          gameTied = SUT.isTied board
      gameTied `shouldBe` True

  describe "getWinningPlayer" $ do
    it "returns blank when neither tied nor won" $ do
      let board = Helper.constructBoard []
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` ""

    it "returns blank when tied" $ do
      let board = Helper.constructBoard [ ("1", "X"), ("2", "O"), ("3", "X")
                                        , ("4", "O"), ("5", "X"), ("6", "O")
                                        , ("7", "O"), ("8", "X"), ("9", "O")]
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` ""

    it "returns winning player marker in top row win" $ do
      let board = Helper.constructBoard [ ("1", "X"), ("2", "X"), ("3", "X") ]
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` "X"

    it "returns winning player marker in mid row win" $ do
      let board = Helper.constructBoard  [("4", "X"), ("5", "X"), ("6", "X")]
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` "X"

    it "returns winning player marker in a bottom-row horizontal match" $ do
      let board = Helper.constructBoard [ ("7", "X"), ("8", "X"), ("9", "X")]
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` "X"

    it "returns winning player marker when board has a left-column vertical match" $ do
      let board = Helper.constructBoard [ ("1", "X")
                                        , ("4", "X")
                                        , ("7", "X")]
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` "X"

    it "returns winning player marker when board has a mid-column vertical match" $ do
      let board = Helper.constructBoard [ ("2", "X")
                                        , ("5", "X")
                                        , ("8", "X")]
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` "X"

    it "returns winning player marker when board has a right-column vertical match" $ do
      let board = Helper.constructBoard [ ("3", "X")
                                        , ("6", "X")
                                        , ("9", "X")]
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` "X"

    it "returns winning player marker when board has a bl - tr diagonal match" $ do
      let board = Helper.constructBoard [ ("3", "X")
                                        , ("5", "X")
                                        , ("7", "X")]
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` "X"

    it "returns winning player marker when board has a tl - br diagonal match" $ do
      let board = Helper.constructBoard [ ("1", "X")
                                        , ("5", "X")
                                        , ("9", "X")]
          winningPlayer = SUT.getWinningPlayer board
      winningPlayer `shouldBe` "X"
