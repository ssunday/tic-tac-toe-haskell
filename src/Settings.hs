module Settings
  (
    askToPlayRound
  , askIfPlayerGoingFirst
  , askForBoardDimension
  , getPlayerMarker
  , getAIMarker
  ) where

import qualified Input as Input
import qualified InputValidation as Validation
import qualified Colors as Colors

askToPlayRound :: IO Bool
askToPlayRound =
  yOrNLoop $ "\nDo you want to play another round of Tic Tac Toe? (yes/no)"

askIfPlayerGoingFirst :: IO Bool
askIfPlayerGoingFirst =
  yOrNLoop $ "\nDo you want to go first? (yes/no)"

askForBoardDimension :: IO Int
askForBoardDimension = go
  where go = do
          dimensionIO <- Input.prompt "\n3x3 board or 4x4 board? 3 for 3x3, 4 for 4x4."
          let dimension = (read dimensionIO)
          if Validation.isBoardDimensionValid dimension
            then return dimension
            else go


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
