module AISpec where

import Test.Hspec

import qualified SpecHelper as Helper

import Markers

import AI as SUT

spec :: Spec
spec = do
  describe "getMove" $ do

    let pl = "O"
    let co = "X"
    let markers = Markers {ai = co, player = pl}

    context "3x3" $ do
     context "intermediate" $ do
       it "returns corner spot on empty board" $ do
         let gameBoard = Helper.constructBoard 3 []
             move = SUT.getMove gameBoard markers
         move `shouldBe` "9"

       it "returns center if top left corner occupied by opponent" $ do
         let gameBoard = Helper.constructBoard 3 [("1", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "5"

       it "returns center if top right corner occupied by opponent" $ do
         let gameBoard = Helper.constructBoard 3 [("3", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "5"

       it "returns center if bottom left corner occupied by opponent" $ do
         let gameBoard = Helper.constructBoard 3 [("7", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "5"

       it "returns center if bottom right corner occupied by opponent" $ do
         let gameBoard = Helper.constructBoard 3 [("9", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "5"

       it "returns bottom left spot when ai occupies bottom right and opponent top left" $ do
         let gameBoard = Helper.constructBoard 3 [("1", pl), ("9", co)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "7"

       it "returns center when it opponent has two middle sides" $ do
         let gameBoard = Helper.constructBoard 3 [("4", pl), ("7", co), ("8", pl), ("9", co)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "5"

       it "returns middle right spot when bottom left is taken" $ do
         let gameBoard = Helper.constructBoard 3 [ ("7", pl), ("9", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "6"

       it "returns bottom middle when top right is taken by player and has bottom right" $ do
         let gameBoard = Helper.constructBoard 3 [ ("3", pl)
                                                 , ("9", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "8"

       it "returns center when opponent has bottom and left side middle and ai has two corners" $ do
         let gameBoard = Helper.constructBoard 3 [ ("4", pl), ("7", co)
                                                 , ("8", pl), ("9", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "5"

     context "wins" $ do
       it "vertically in first column" $ do
         let gameBoard = Helper.constructBoard 3 [ ("1", co), ("4", co)
                                                 , ("5", pl), ("9", pl) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "7"

       it "vertically in second column" $ do
         let gameBoard = Helper.constructBoard 3 [ ("3", pl), ("5", co)
                                                 , ("7", pl), ("8", co)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "2"

       it "in third column instead of blocking in middle" $ do
         let gameBoard = Helper.constructBoard 3 [ ("1", co), ("2", pl), ("3", co)
                                                 , ("4", co), ("5", pl)
                                                 , ("7", pl), ("9", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "6"

       it "in diagonal case TL-BR - first spot" $ do
         let gameBoard = Helper.constructBoard 3 [ ("2", pl), ("5", co)
                                                 , ("7", pl), ("8", pl), ("9", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "1"

       it "in diagonal case TL-BR - br spot" $ do
         let gameBoard = Helper.constructBoard 3 [ ("1", co), ("2", pl)
                                                 , ("3", pl), ("5", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "9"

       it "over blocking" $ do
         let gameBoard = Helper.constructBoard 3 [ ("1", pl), ("3", pl)
                                                 , ("7", co), ("9", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "8"

     context "blocks" $ do
       it "in bl-tr diagonal case" $ do
         let gameBoard = Helper.constructBoard 3 [ ("2", co), ("5", pl)
                                                 , ("7", pl), ("8", pl), ("9", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "3"

       it "in tl-br diagonal case" $ do
         let gameBoard = Helper.constructBoard 3 [ ("1", pl), ("2", co)
                                                 , ("4", co), ("5", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "9"

       it "in top row" $ do
         let gameBoard = Helper.constructBoard 3 [ ("1", pl), ("3", pl)
                                                 , ("5", co)
                                                 , ("9", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "2"

       it "in first column" $ do
         let gameBoard = Helper.constructBoard 3 [ ("1", pl), ("2", pl), ("3", co)
                                                 , ("5", co), ("7", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "4"

       it "in middle column" $ do
         let gameBoard = Helper.constructBoard 3 [ ("3", pl),
                                                   ("5", pl), ("6", co)
                                                 , ("7", co), ("8", pl), ("9", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "2"

       it "in third column" $ do
         let gameBoard = Helper.constructBoard 3 [("3", pl), ("5", co), ("6", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "9"

    context "4x4" $ do
     context "intermediate" $ do
       it "returns side spot on empty board" $ do
         let gameBoard = Helper.constructBoard 4 []
             move = SUT.getMove gameBoard markers
         move `shouldBe` "9"

     context "wins" $ do
       it "vertically" $ do
         let gameBoard = Helper.constructBoard 4 [ ("1", co)
                                                 , ("5", co)
                                                 , ("9", co), ("10", pl)
                                                 , ("15", pl), ("16", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "13"

       it "horizontally" $ do
         let gameBoard = Helper.constructBoard 4 [ ("5", co), ("6", co), ("8", co)
                                                  , ("9", pl), ("11", co), ("12", pl)
                                                  , ("13", pl), ("16", pl) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "7"

       it "wins in diagonal case" $ do
         let gameBoard = Helper.constructBoard 4 [ ("6", co), ("8", pl)
                                                 , ("11", co), ("12", pl)
                                                 , ("13", pl), ("16", co) ]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "1"

       it "prioritizes winning over blocking" $ do
         let gameBoard = Helper.constructBoard 4 [ ("1", pl), ("2", co)
                                                 , ("5", pl), ("6", co)
                                                 , ("9", pl), ("10", co)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "14"

     context "blocks" $ do
       it "in diagonal case" $ do
         let gameBoard = Helper.constructBoard 4 [ ("1", pl), ("2", co)
                                                 , ("4", co), ("5", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "9"

       it "vertically" $ do
         let gameBoard = Helper.constructBoard 4 [ ("1", pl), ("2", pl), ("4", co)
                                                 , ("5", co), ("6", pl), ("7", co)
                                                 , ("9", co), ("10", pl)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "14"

       it "horizontally" $ do
         let gameBoard = Helper.constructBoard 4 [ ("1", pl), ("2", pl), ("3", pl)
                                                 , ("5", co), ("7", co)
                                                 , ("9", pl), ("11", co), ("12", co)]
             move = SUT.getMove gameBoard markers
         move `shouldBe` "4"
