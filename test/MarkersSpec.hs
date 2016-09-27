module MarkersSpec where

import Test.Hspec
import Test.QuickCheck

import Markers

main :: IO ()
main = hspec spec

spec :: Spec

spec = do
  describe "Markers" $ do
    it "assigns player marker to player" $ property $ \x y->
      let markers = Markers {ai = y, player = x}
          playerMarker = (player markers)
      in playerMarker == x

    it "assigns first marker to ai" $ property $ \x y->
      let markers = Markers {ai = y, player = x}
          aiMarker = (ai markers)
      in aiMarker == y
