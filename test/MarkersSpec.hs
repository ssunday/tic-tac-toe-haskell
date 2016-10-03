module MarkersSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Markers

spec :: Spec
spec = do
  describe "Markers" $ do
    prop "assigns player marker to player" $ \markerOne markerTwo ->
      let markers = Markers {ai = markerOne, player = markerTwo}
          playerMarker = (player markers)
      in playerMarker == markerTwo

    prop "assigns first marker to ai" $ \markerOne markerTwo ->
      let markers = Markers {ai = markerOne, player = markerTwo}
          aiMarker = (ai markers)
      in aiMarker == markerOne
