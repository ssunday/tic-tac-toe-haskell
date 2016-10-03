module InputValidation
  (
    isYes
  , isValidYOrNInput
  , isMarkerValid
  ) where

isYes :: String -> Bool
isYes input =
  input == "y"

isValidYOrNInput :: String -> Bool
isValidYOrNInput input =
  input == "y" || input == "n"

isMarkerValid :: String -> Bool
isMarkerValid marker =
  length marker == 1
