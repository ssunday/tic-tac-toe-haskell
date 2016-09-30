module Settings
  (
    getPlayerMarker
  , getAIMarker
  , playRound
  , isMarkerValid
  ) where

import qualified Input as Input
import qualified Colors as Colors

playRound :: IO Bool
playRound = do
  play <- Input.prompt $ "\nDo you want to play a round of Tic Tac Toe? (y/n)"
  return $ play /= "n"

getPlayerMarker :: IO String
getPlayerMarker =
  getMarker "\nWhat do you want your marker to be? (Length of one)" "LIGHT CYAN"

getAIMarker :: IO String
getAIMarker =
  getMarker "\nWhat do you want the computer marker to be? (Length of one)" "LIGHT RED"

getMarker :: String -> String -> IO String
getMarker prompt markerColor = go
  where go = do
          markerIO <- Input.prompt prompt
          let marker = markerIO :: String
          if (isMarkerValid marker)
            then return $ Colors.colorString markerColor marker
            else go

isMarkerValid :: String -> Bool
isMarkerValid marker =
  length marker == 1
