module AISpec where

import Test.Hspec

import qualified Data.Map as Map

import Markers

import AI as SUT

main :: IO ()
main = hspec spec

spec :: Spec

pl :: String
pl = "O"
co :: String
co = "X"

markers :: Markers String String
markers = Markers {ai = co, player = pl}

spec = do
  describe "getMove" $ do

    context "intermediate" $ do
      it "returns corner spot on empty board" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                                     , ("4", ""), ("5", ""), ("6", "")
                                     , ("7", ""), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "9"

      it "returns center if top left corner occupied by opponent" $ do
        let gameBoard = Map.fromList [ ("1",pl), ("2", ""), ("3", "")
                                     , ("4", ""), ("5", ""), ("6", "")
                                     , ("7", ""), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "5"

      it "returns center if top right corner occupied by opponent" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", pl)
                                     , ("4", ""), ("5", ""), ("6", "")
                                     , ("7", ""), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "5"


      it "returns center if bottom left corner occupied by opponent" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                                     , ("4", ""), ("5", ""), ("6", "")
                                     , ("7", pl), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "5"

      it "returns center if bottom right corner occupied by opponent" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                                     , ("4", ""), ("5", ""), ("6", "")
                                     , ("7", ""), ("8", ""), ("9", pl) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "5"

      it "returns bottom left spot when ai occupies bottom right and opponent top left" $ do
        let gameBoard = Map.fromList [ ("1",pl), ("2", ""), ("3", "")
                                     , ("4", ""), ("5", ""), ("6", "")
                                     , ("7", ""), ("8", ""), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "7"

      it "returns center when it opponent has two middle sides" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                                     , ("4",pl), ("5", ""), ("6", "")
                                     , ("7", co), ("8",pl), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "5"

      it "returns middle right spot when bottom left is taken" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                                     , ("4", ""), ("5", ""), ("6", "")
                                     , ("7", pl), ("8",""), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "6"

      it "returns bottom middle when top right is taken by player and has bottom right" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", pl)
                                     , ("4", ""), ("5", ""), ("6", "")
                                     , ("7", ""), ("8",""), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "8"

      it "returns center when opponent has bottom and left side middle and ai has two corners" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                                     , ("4", pl), ("5", ""), ("6", "")
                                     , ("7", co), ("8", pl), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "5"

    context "wins" $ do
      it "wins vertically in first column" $ do
        let gameBoard = Map.fromList [ ("1", co), ("2", ""), ("3", "")
                                     , ("4", co), ("5", pl), ("6", "")
                                     , ("7", ""), ("8", ""), ("9",pl) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "7"

      it "wins vertically in second column" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", pl)
                                     , ("4", ""), ("5", co), ("6", "")
                                     , ("7", pl), ("8", co), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "2"

      it "wins in third column instead of blocking in middle" $ do
        let gameBoard = Map.fromList [ ("1", co), ("2", pl), ("3", co)
                                     , ("4", co), ("5", pl), ("6", "")
                                     , ("7", pl), ("8", ""), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "6"

      it "wins in diagonal case TL-BR - first spot" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", pl), ("3", "")
                                     , ("4", ""), ("5", co), ("6", "")
                                     , ("7", pl), ("8", pl), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "1"

      it "wins in diagonal case TL-BR - br spot" $ do
        let gameBoard = Map.fromList [ ("1", co), ("2", pl), ("3", pl)
                                     , ("4", ""), ("5", co), ("6", "")
                                     , ("7", ""), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "9"

  -- (it "wins when spot five is open and has marked 3 and 4"
  --   (should= 5 (best-move [pl pl ai
  --                          ai ai 5
  --                          pl pl 8] ai pl)))

  -- (it "blocks opponent from winning in diagonal case TL-BR"
  --   (should= 8 (best-move [pl ai 2
  --                          3 pl 5
  --                          ai 7 8] ai pl)))

    it "prioritizes winning over blocking" $ do
      let gameBoard = Map.fromList [ ("1",pl), ("2", ""), ("3",pl)
                                   , ("4", ""), ("5", ""), ("6", "")
                                   , ("7", co), ("8", ""), ("9", co) ]
          move = SUT.getMove gameBoard markers
      move `shouldBe` "8"


    context "blocks" $ do
      it "in bl-tr diagonal case" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", co), ("3", "")
                                     , ("4", ""), ("5", pl), ("6", "")
                                     , ("7", pl), ("8", pl), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "3"

      it "in tl-br diagonal case" $ do
        let gameBoard = Map.fromList [ ("1",pl), ("2", co), ("3", "")
                                     , ("4", co), ("5",pl), ("6", "")
                                     , ("7", ""), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "9"

      it "in top row - right" $ do
        let gameBoard = Map.fromList [ ("1",pl), ("2",pl), ("3", "")
                                     , ("4", ""), ("5", co), ("6", "")
                                     , ("7", ""), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "3"

      it "in top row - center" $ do
        let gameBoard = Map.fromList [ ("1",pl), ("2", ""), ("3",pl)
                                     , ("4", ""), ("5", co), ("6", "")
                                     , ("7", ""), ("8", ""), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "2"

      it "in first column" $ do
        let gameBoard = Map.fromList [ ("1",pl), ("2",pl), ("3", co)
                                     , ("4", ""), ("5", co), ("6", "")
                                     , ("7",pl), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "4"

      it "in middle column - top" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3", pl)
                                     , ("4", ""), ("5", pl), ("6", co)
                                     , ("7", co), ("8", pl), ("9", co) ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "2"

      it " middle column - bottom" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", pl), ("3", co)
                                     , ("4", ""), ("5", pl), ("6", "")
                                     , ("7", co), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "8"

      it "in third column" $ do
        let gameBoard = Map.fromList [ ("1", ""), ("2", ""), ("3",pl)
                                     , ("4", ""), ("5", co), ("6",pl)
                                     , ("7", ""), ("8", ""), ("9", "") ]
            move = SUT.getMove gameBoard markers
        move `shouldBe` "9"
