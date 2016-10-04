module ScoreTXTSpec where

import Test.Hspec

import qualified System.Directory as Directory
import qualified System.IO as IO

import qualified ScoreTXT as SUT

scoreFileTest :: String
scoreFileTest = "winners-test.txt"

clearFile :: IO ()
clearFile = do
  doesExist <- Directory.doesFileExist scoreFileTest
  if (doesExist :: Bool)
    then Directory.removeFile scoreFileTest
    else putStr ""

spec :: Spec
spec = after_ clearFile $ do
  describe "recordWinner" $ do
    it "writes the winning player marker to a file with a comma after it" $ do
      let marker = "X"
      _ <- SUT.recordWinner marker scoreFileTest
      contents <- (IO.readFile scoreFileTest)
      (contents :: String) `shouldBe` (marker ++ ",")

    it "can write multiple winning player markers to a file and persists all of them, comma separated" $ do
      let marker = "X"
      _ <- SUT.recordWinner marker scoreFileTest
      _ <- SUT.recordWinner marker scoreFileTest
      contents <- (IO.readFile scoreFileTest)
      (contents :: String) `shouldBe` (marker ++ "," ++ marker ++ ",")

  describe "getWinners" $ do
    it "gets the winner from the file" $ do
      let marker = "X"
      _ <- IO.appendFile scoreFileTest (marker ++ ",")
      contents <- SUT.getWinners scoreFileTest
      (contents :: String) `shouldBe` (marker ++ ",")

    it "gets multiple winners" $ do
      let marker = "X"
      _ <- IO.appendFile scoreFileTest (marker ++ ",")
      _ <- IO.appendFile scoreFileTest (marker ++ ",")
      contents <- SUT.getWinners scoreFileTest
      (contents :: String) `shouldBe` (marker ++ "," ++ marker ++ ",")

    it "gets blank when file does not exist" $ do
      contents <- SUT.getWinners "blarg.txt"
      (contents :: String) `shouldBe` ""
