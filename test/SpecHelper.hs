module SpecHelper
  (
    constructBoard
  , extractValueFromMap
  )
where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

baseMap :: Map.Map String String
baseMap = Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                       , ("4", ""), ("5", ""), ("6", "")
                       , ("7", ""), ("8", ""), ("9", "")]

constructBoard :: [(String, String)] -> Map.Map String String
constructBoard [] = baseMap
constructBoard spotOverrides =
  Map.union (Map.fromList spotOverrides) baseMap

extractValueFromMap :: Map.Map String String -> String -> String
extractValueFromMap mapToExtract key =
  fromMaybe "" $ Map.lookup key mapToExtract
