module ScoreSpec where

import Test.Hspec

import qualified System.IO as IO

import qualified Score as SUT

main :: IO ()
main = hspec spec

spec :: Spec

spec = do
  describe "recordWinner" $ do
    it "writes the winning player marker to a file with a comma after it" $ do
      let marker = "X"
      _ <- SUT.recordWinner marker SUT.scoreFileTest
      contents <- (IO.readFile SUT.scoreFileTest)
      _ <- SUT.clearFile SUT.scoreFileTest
      (contents :: String) `shouldBe` (marker ++ ",")
    it "writes the blank marker as 'TIE'" $ do
      let marker = ""
      _ <- SUT.recordWinner marker SUT.scoreFileTest
      contents <- (IO.readFile SUT.scoreFileTest)
      _ <- SUT.clearFile SUT.scoreFileTest
      (contents :: String) `shouldBe` "TIE,"
    it "can write multiple winning player markers to a file and persists all of them, comma separated" $ do
      let marker = "X"
      _ <- SUT.recordWinner marker SUT.scoreFileTest
      _ <- SUT.recordWinner marker SUT.scoreFileTest
      contents <- (IO.readFile SUT.scoreFileTest)
      _ <- SUT.clearFile SUT.scoreFileTest
      (contents :: String) `shouldBe` (marker ++ "," ++ marker ++ ",")

  describe "getWinners" $ do
    it "gets the winner from the file" $ do
      let marker = "X"
      _ <- IO.appendFile SUT.scoreFileTest (marker ++ ",")
      contents <- SUT.getWinners SUT.scoreFileTest
      _ <- SUT.clearFile SUT.scoreFileTest
      (contents :: String) `shouldBe` (marker ++ ",")
    it "gets multiple winners" $ do
      let marker = "X"
      _ <- IO.appendFile SUT.scoreFileTest (marker ++ ",")
      _ <- IO.appendFile SUT.scoreFileTest (marker ++ ",")
      contents <- SUT.getWinners SUT.scoreFileTest
      _ <- SUT.clearFile SUT.scoreFileTest
      (contents :: String) `shouldBe` (marker ++ "," ++ marker ++ ",")

  describe "getTallys" $ do
    it "creates list of tuples of with the first element as the marker and second as occurrences of comma seperated list" $ do
      let markers = "X,X,Y,"
          occurrences = SUT.getTallys markers
      occurrences `shouldBe` [("X", 2), ("Y", 1)]

    it "maintains order and uniqueness" $ do
      let markers = "X,Z,Y,W,X,Y,P,X"
          occurrences = SUT.getTallys markers
      occurrences `shouldBe` [("X", 3), ("Z", 1), ("Y", 2), ("W", 1), ("P", 1)]

  describe "doesScoreFileExist" $ do
    it "returns false when file does not exist" $ do
      exists <- SUT.doesScoreFileExist "werwerer"
      (exists :: Bool) `shouldBe` False

    it "returns true when file does exists" $ do
      _ <- IO.appendFile SUT.scoreFileTest "blah"
      exists <- SUT.doesScoreFileExist SUT.scoreFileTest
      _ <- SUT.clearFile SUT.scoreFileTest
      (exists :: Bool) `shouldBe` True
