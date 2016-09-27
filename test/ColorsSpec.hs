module ColorsSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.List as List

import qualified Colors as SUT

main :: IO ()
main = hspec spec

spec :: Spec

spec = do
  describe "colorString" $ do
    it "returns string starting with correct color" $ property $ \x ->
      let color = "PURPLE"
          colorCode = "\x1b[35m"
          coloredString = SUT.colorString color x
      in List.isPrefixOf colorCode coloredString

    it "ends with white" $ property $ \x ->
      let color = "PURPLE"
          white = "\x1b[0m"
          coloredString = SUT.colorString color x
      in List.isSuffixOf white coloredString

    it "defaults to white if color does not exist" $ property $ \x ->
      let color = "I am fake"
          colorCode = "\x1b[0m"
          coloredString = SUT.colorString color x
      in List.isPrefixOf colorCode coloredString
