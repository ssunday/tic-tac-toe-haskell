module SpecHelperSpec where

import Test.Hspec

import qualified Data.Map as Map

import SpecHelper as SUT

spec :: Spec
spec = do
  describe "constructBoard" $ do
    it "returns a map of a blank board with empty map" $ do
      let baseMap = []
          testMap = SUT.constructBoard baseMap
       in testMap `shouldBe` Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                                          , ("4", ""), ("5", ""), ("6", "")
                                          , ("7", ""), ("8", ""), ("9", "")]

    it "returns a map of a board with provided spots taken" $ do
      let baseMap = [ ("3", "X")
                    , ("7", "O")]
          testMap = SUT.constructBoard baseMap
       in testMap `shouldBe` Map.fromList [ ("1", ""), ("2", ""), ("3", "X")
                                          , ("4", ""), ("5", ""), ("6", "")
                                          , ("7", "O"), ("8", ""), ("9", "")]

  describe "extractValueFromMap" $ do
    it "returns the value of the key of the given map" $ do
      let mapToExtract = Map.fromList [("1", "O"), ("2", "X"), ("3", "Z")]
          value = SUT.extractValueFromMap mapToExtract "3"
      value `shouldBe` "Z"
