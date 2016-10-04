module Settings
  (
    getPlayerMarker
  , getAIMarker
  , askToPlayRound
  , askIfPlayerGoingFirst
  ) where

import qualified Input as Input
import qualified InputValidation as Validation
import qualified Colors as Colors

askToPlayRound :: IO Bool
askToPlayRound =
  yOrNLoop $ "\nDo you want to play another round of Tic Tac Toe? (y/n)"

askIfPlayerGoingFirst :: IO Bool
askIfPlayerGoingFirst =
  yOrNLoop $ "\nDo you want to go first? (y/n)"

getPlayerMarker :: IO String
getPlayerMarker =
  getMarker "\nWhat do you want your marker to be? (Any single character)" "LIGHT CYAN"

getAIMarker :: IO String
getAIMarker =
  getMarker "\nWhat do you want the computer marker to be? (Any single character)" "LIGHT RED"

getMarker :: String -> String -> IO String
getMarker prompt markerColor = go
  where go = do
          markerIO <- Input.prompt prompt
          let marker = markerIO :: String
          if Validation.isMarkerValid marker
            then return $ Colors.colorString markerColor marker
            else go

yOrNLoop :: String -> IO Bool
yOrNLoop promptString = go
  where go = do
          responseIO <- Input.prompt promptString
          let response = responseIO :: String
          if Validation.isValidYOrNInput response
            then return $ Validation.isYes response
            else go
