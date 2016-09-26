module GameLogicSpec where

import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import GameLogic

main :: IO ()
main = hspec spec

spec :: Spec

spec = do
  describe "Board" $ do
    it "returns a map of a blank board " $ do
      board  `shouldBe` Map.fromList [ ("1", "")
                                     , ("2", "")
				     , ("3", "")
				     , ("4", "")
				     , ("5", "")
				     , ("6", "")
				     , ("7", "")
				     , ("8", "")
				     , ("9", "")]
  describe "Mark Board" $ do
    it "returns board with given spot marked with token " $ do
      let gameBoard = board
          spot = "2"
          mark = "X"
          markedBoard = markBoard gameBoard spot mark
          spotMarked =  Map.lookup spot markedBoard
      Maybe.fromMaybe "" spotMarked `shouldBe` mark
