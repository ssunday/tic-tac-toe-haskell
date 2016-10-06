module InputValidation
  (
    isYes
  , isValidYOrNInput
  , isMarkerValid
  , isMenuOptionValid
  , isBoardDimensionValid
  ) where

isYes :: String -> Bool
isYes input =
  elem input ["y", "yes", "aye"]

isValidYOrNInput :: String -> Bool
isValidYOrNInput input =
  elem input ["y", "n", "yes" , "no", "aye", "bashl"]

isMarkerValid :: String -> Bool
isMarkerValid marker =
  length marker == 1

isBoardDimensionValid :: Int -> Bool
isBoardDimensionValid dimension =
  elem dimension [3, 4]

isMenuOptionValid :: [(Int, String)] -> Int -> Bool
isMenuOptionValid menu option =
  option > 0 && option < (length menu) + 1
