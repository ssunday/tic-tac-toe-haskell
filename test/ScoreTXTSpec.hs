module ScoreTXTSpec where

import Test.Hspec

import qualified System.IO as IO

import qualified ScoreTXT as SUT
import qualified SpecHelper as Helper

scoreFileTest :: String
scoreFileTest = "winners-test.txt"

spec :: Spec
spec = after_ (Helper.deleteFile scoreFileTest) $ do
  describe "recordWinner" $ do
    it "writes the winning player marker to a file with a comma after it" $ do
      let marker = "X"
      SUT.recordWinner marker scoreFileTest
      contents <- IO.readFile scoreFileTest
      (contents :: String) `shouldBe` (marker ++ ",")

    it "can write multiple winning player markers to a file and persists all of them, comma separated" $ do
      let marker = "X"
      SUT.recordWinner marker scoreFileTest
      SUT.recordWinner marker scoreFileTest
      contents <- IO.readFile scoreFileTest
      (contents :: String) `shouldBe` (marker ++ "," ++ marker ++ ",")

  describe "getWinners" $ do
    it "gets the winner from the file" $ do
      let marker = "X"
      IO.appendFile scoreFileTest (marker ++ ",")
      contents <- SUT.getWinners scoreFileTest
      (contents :: String) `shouldBe` (marker ++ ",")

    it "gets multiple winners" $ do
      let marker = "X"
      IO.appendFile scoreFileTest (marker ++ ",")
      IO.appendFile scoreFileTest (marker ++ ",")
      contents <- SUT.getWinners scoreFileTest
      (contents :: String) `shouldBe` (marker ++ "," ++ marker ++ ",")

    it "returns blank string when file does not exist" $ do
      contents <- SUT.getWinners "blarg.txt"
      (contents :: String) `shouldBe` ""
