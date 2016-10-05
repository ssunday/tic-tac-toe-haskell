module SpecHelperSpec where

import Test.Hspec

import qualified Data.Map as Map
import qualified System.Directory as Directory

import SpecHelper as SUT

spec :: Spec
spec = do
  describe "constructBoard" $ do
    it "returns a map of a blank 3x3 board with empty map with 3 passed in" $ do
      let overrides = []
          testMap = SUT.constructBoard 3 overrides
       in testMap `shouldBe` Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                                          , ("4", ""), ("5", ""), ("6", "")
                                          , ("7", ""), ("8", ""), ("9", "")]

    it "returns a map of a blank 4x4 board with empty map with 4 passed in" $ do
      let overrides = []
          testMap = SUT.constructBoard 4 overrides
       in testMap `shouldBe` Map.fromList [ ("1", ""), ("2", ""), ("3", ""), ("4", "")
                                          , ("5", ""), ("6", ""), ("7", ""), ("8", "")
                                          , ("9", ""), ("10", ""), ("11", ""), ("12", "")
                                          , ("13", ""), ("14", ""), ("15", ""), ("16", "")]

    it "returns a map of a 3x3 board with provided spots taken" $ do
      let overrides = [ ("3", "X")
                      , ("7", "O")]
          testMap = SUT.constructBoard 3 overrides
       in testMap `shouldBe` Map.fromList [ ("1", ""), ("2", ""), ("3", "X")
                                          , ("4", ""), ("5", ""), ("6", "")
                                          , ("7", "O"), ("8", ""), ("9", "")]

    it "returns a map of a 4x4 board with provided spots marked" $ do
      let overrides = [ ("3", "X")
                      , ("7", "O")
                      , ("14", "O")]
          testMap = SUT.constructBoard 4 overrides
       in testMap `shouldBe` Map.fromList [ ("1", ""), ("2", ""), ("3", "X"), ("4", "")
                                          , ("5", ""), ("6", ""), ("7", "O"), ("8", "")
                                          , ("9", ""), ("10", ""), ("11", ""), ("12", "")
                                          , ("13", ""), ("14", "O"), ("15", ""), ("16", "")]

  describe "valueFromKey" $ do
    it "returns the value of the key of the given map" $ do
      let mapToExtract = Map.fromList [("1", "O"), ("2", "X"), ("3", "Z")]
          value = SUT.valueFromKey mapToExtract "3"
      value `shouldBe` "Z"

  describe "deleteFile" $ do
    it "file still does not exist when deleting file that does not exist" $ do
      let fakeFile = "blah.txt"
      SUT.deleteFile fakeFile
      doesExist <- Directory.doesFileExist fakeFile
      (doesExist :: Bool) `shouldBe` False
