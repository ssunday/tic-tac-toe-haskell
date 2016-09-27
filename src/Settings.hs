module Settings
  (
    getPlayerMarker
  , getAIMarker
  , isMarkerValid
  ) where

import qualified Input as I

getPlayerMarker :: IO String
getPlayerMarker = go
  where go = do
          playerMarkerIO <- I.prompt "What do you want your Marker to be? (Length of one)"
          let playerMarker = playerMarkerIO :: String
          if (isMarkerValid playerMarker)
            then return $ "\x1b[96m" ++ playerMarker ++ "\x1b[0m"
            else go

getAIMarker :: String -> IO String
getAIMarker otherMarker = go otherMarker
  where go other = do
          aiMarkerIO <- I.prompt "What do you want the AI Marker to be? (Do not make it same as your marker and be length of one)"
          let aiMarker = aiMarkerIO :: String
          if (isMarkerValid aiMarker) && (not (aiMarker == other))
            then return $ "\x1b[91m" ++ aiMarker ++ "\x1b[0m"
            else go other

isMarkerValid :: String -> Bool
isMarkerValid marker =
  length marker == 1
