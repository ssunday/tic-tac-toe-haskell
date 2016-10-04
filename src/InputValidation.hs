module InputValidation
  (
    isYes
  , isValidYOrNInput
  , isMarkerValid
  , isMenuOptionValid
  ) where

isYes :: String -> Bool
isYes input =
  elem input ["y", "yes"]

isValidYOrNInput :: String -> Bool
isValidYOrNInput input =
  elem input ["y", "n", "yes" , "no"]

isMarkerValid :: String -> Bool
isMarkerValid marker =
  length marker == 1

isMenuOptionValid :: [(Int, String)] -> Int -> Bool
isMenuOptionValid menu option =
  option > 0 && option < (length menu) + 1
