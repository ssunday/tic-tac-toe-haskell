module GameLogicSpec where

import Test.Hspec

import qualified SpecHelper as Helper

import GameLogic as SUT

spec :: Spec
spec = do
  describe "isOver" $ do
    context "3x3" $ do
      it "returns false when game is not over" $ do
        let board = Helper.constructBoard 3 []
            isGameOver = SUT.isOver board
        isGameOver `shouldBe` False

      it "returns true when game has been won" $ do
        let board = Helper.constructBoard 3 [ ("1", "X")
                                            , ("4", "O"), ("5", "X")
                                            , ("8", "O"), ("9", "X")]
            isGameOver = SUT.isOver board
        isGameOver `shouldBe` True

      it "returns true when game has been tied" $ do
        let board = Helper.constructBoard 3 [ ("1", "X"), ("2", "X"), ("3", "O")
                                            , ("4", "O"), ("5", "X"), ("6", "X")
                                            , ("7", "X"), ("8", "O"), ("9", "O")]
            isGameOver = SUT.isOver board
        isGameOver `shouldBe` True

    context "4x4" $ do
      it "returns false when game is not over" $ do
        let board = Helper.constructBoard 4 []
            isGameOver = SUT.isOver board
        isGameOver `shouldBe` False

      it "returns true when game has been won" $ do
        let board = Helper.constructBoard 4 [ ("1", "X")
                                            , ("5", "X"), ("6", "O")
                                            , ("9", "X"), ("10", "O")
                                            , ("13", "X"), ("14", "O")]
            isGameOver = SUT.isOver board
        isGameOver `shouldBe` True

      it "returns true when game has been tied" $ do
        let board = Helper.constructBoard 4 [ ("1", "X"), ("2", "O"), ("3", "X"), ("4", "O")
                                            , ("5", "X"), ("6", "O"), ("7", "X"), ("8", "O")
                                            , ("9", "X"), ("10", "O"), ("11", "X"), ("12", "O")
                                            , ("13", "O"), ("14", "X"), ("15", "O"), ("16", "X")]
            isGameOver = SUT.isOver board
        isGameOver `shouldBe` True

  describe "isTied" $ do
    context "3x3" $ do
      it "returns false when board is not tied" $ do
        let board = Helper.constructBoard 3 [ ("1", "X"), ("2", "O")
                                            , ("4", "O")
                                            , ("8", "X")]
            gameTied = SUT.isTied board
        gameTied `shouldBe` False

      it "returns false when board is won" $ do
        let board = Helper.constructBoard 3 [ ("1", "X"), ("2", "O"), ("3", "X")
                                            , ("4", "O"), ("5", "X")
                                            , ("7", "X"), ("8", "X")]
            gameTied = SUT.isTied board
        gameTied `shouldBe` False

      it "returns true when board is tied" $ do
        let board = Helper.constructBoard 3 [ ("1", "X"), ("2", "O"), ("3", "X")
                                            , ("4", "O"), ("5", "X"), ("6", "O")
                                            , ("7", "O"), ("8", "X"), ("9", "O")]
            gameTied = SUT.isTied board
        gameTied `shouldBe` True

    context "4x4" $ do
      it "returns false when board is not tied" $ do
        let board = Helper.constructBoard 4 [ ("1", "X"), ("2", "O")
                                            , ("4", "O")
                                            , ("8", "X")]
            gameTied = SUT.isTied board
        gameTied `shouldBe` False

      it "returns false when board is won" $ do
        let board = Helper.constructBoard 4 [ ("1", "X")
                                            , ("5", "X"), ("6", "O")
                                            , ("9", "X"), ("10", "O")
                                            , ("13", "X"), ("14", "O")]
            gameTied = SUT.isTied board
        gameTied `shouldBe` False

      it "returns true when board is tied" $ do
        let board = Helper.constructBoard 4 [ ("1", "X"), ("2", "O"), ("3", "X"), ("4", "O")
                                            , ("5", "X"), ("6", "O"), ("7", "X"), ("8", "O")
                                            , ("9", "X"), ("10", "O"), ("11", "X"), ("12", "O")
                                            , ("13", "O"), ("14", "X"), ("15", "O"), ("16", "X")]
            gameTied = SUT.isTied board
        gameTied `shouldBe` True

  describe "getWinningPlayer" $ do
    context "3x3" $ do
      it "returns blank when neither tied nor won" $ do
        let board = Helper.constructBoard 3 []
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` ""

      it "returns blank when tied" $ do
        let board = Helper.constructBoard 3 [ ("1", "X"), ("2", "O"), ("3", "X")
                                            , ("4", "O"), ("5", "X"), ("6", "O")
                                            , ("7", "O"), ("8", "X"), ("9", "O")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` ""

      it "returns winning player marker in top row win" $ do
        let board = Helper.constructBoard 3 [ ("1", "X"), ("2", "X"), ("3", "X") ]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker in mid row win" $ do
        let board = Helper.constructBoard 3  [("4", "X"), ("5", "X"), ("6", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker in a bottom-row horizontal match" $ do
        let board = Helper.constructBoard 3 [ ("7", "X"), ("8", "X"), ("9", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a left-column vertical match" $ do
        let board = Helper.constructBoard 3 [ ("1", "X")
                                            , ("4", "X")
                                            , ("7", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a mid-column vertical match" $ do
        let board = Helper.constructBoard 3 [ ("2", "X")
                                            , ("5", "X")
                                            , ("8", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a right-column vertical match" $ do
        let board = Helper.constructBoard 3 [ ("3", "X")
                                            , ("6", "X")
                                            , ("9", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a bl - tr diagonal match" $ do
        let board = Helper.constructBoard 3 [ ("3", "X")
                                            , ("5", "X")
                                            , ("7", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a tl - br diagonal match" $ do
        let board = Helper.constructBoard 3 [ ("1", "X")
                                            , ("5", "X")
                                            , ("9", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

    context "4x4" $ do
      it "returns blank when neither tied nor won" $ do
        let board = Helper.constructBoard 4 []
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` ""

      it "returns blank when tied" $ do
        let board = Helper.constructBoard 4 [ ("1", "X"), ("2", "O"), ("3", "X"), ("4", "O")
                                            , ("5", "X"), ("6", "O"), ("7", "X"), ("8", "O")
                                            , ("9", "X"), ("10", "O"), ("11", "X"), ("12", "O")
                                            , ("13", "O"), ("14", "X"), ("15", "O"), ("16", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` ""

      it "returns winning player marker in top row win" $ do
        let board = Helper.constructBoard 4 [ ("1", "X"), ("2", "X"), ("3", "X"), ("4", "X") ]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker in top-mid row win" $ do
        let board = Helper.constructBoard 4  [("5", "X"), ("6", "X"), ("7", "X"), ("8", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker in a bottom-mid-row horizontal match" $ do
        let board = Helper.constructBoard 4 [("9", "X"), ("10", "X"), ("11", "X"), ("12", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker in a bottom-row win" $ do
        let board = Helper.constructBoard 4 [("13", "X"), ("14", "X"), ("15", "X"), ("16", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a left-column vertical match" $ do
        let board = Helper.constructBoard 4 [ ("1", "X")
                                            , ("5", "X")
                                            , ("9", "X")
                                            , ("13", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a mid-left-column vertical match" $ do
        let board = Helper.constructBoard 4 [ ("2", "X")
                                            , ("6", "X")
                                            , ("10", "X")
                                            , ("14", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a mid-right-column vertical match" $ do
        let board = Helper.constructBoard 4 [ ("3", "X")
                                            , ("7", "X")
                                            , ("11", "X")
                                            , ("15", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a right-column vertical match" $ do
        let board = Helper.constructBoard 4 [ ("4", "X")
                                            , ("8", "X")
                                            , ("12", "X")
                                            , ("16", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a bl - tr diagonal match" $ do
        let board = Helper.constructBoard 4 [ ("4", "X")
                                            , ("7", "X")
                                            , ("10", "X")
                                            , ("13", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"

      it "returns winning player marker when board has a tl - br diagonal match" $ do
        let board = Helper.constructBoard 4 [ ("1", "X")
                                            , ("6", "X")
                                            , ("11", "X")
                                            , ("16", "X")]
            winningPlayer = SUT.getWinningPlayer board
        winningPlayer `shouldBe` "X"
