module Score
  (
    scoreFile
  , scoreFileTest
  , doesScoreFileExist
  , getWinners
  , recordWinner
  , getTallys
  , clearFile
  )
where

import qualified Data.List as List (nub)
import qualified Data.List.Split as Split
import qualified System.IO as IO
import qualified System.Directory as Directory

scoreFile :: String
scoreFile = "winners.txt"

scoreFileTest :: String
scoreFileTest = "winners-test.txt"

doesScoreFileExist :: String -> IO Bool
doesScoreFileExist file =
  Directory.doesFileExist file

recordWinner :: String -> String -> IO()
recordWinner winningPlayer file =
  IO.appendFile file $ markerToRecord winningPlayer

markerToRecord :: String -> String
markerToRecord [] = "TIE,"
markerToRecord marker = marker ++ ","

getTallys :: String -> [(String, Int)]
getTallys winners =
  let split = splitByComma winners
  in tallyWinners split

getWinners :: String -> IO String
getWinners file =
  IO.readFile file

splitByComma :: String -> [String]
splitByComma commaString =
  filter (not . null) split
  where
    split = Split.splitOn "," commaString

tallyWinners :: [String] -> [(String, Int)]
tallyWinners winningMarkers =
  zip uniqueMarkers occurrences
  where
    uniqueMarkers = List.nub winningMarkers
    occurrences = map (\marker -> length (filter (== marker) winningMarkers)) uniqueMarkers

clearFile :: String -> IO()
clearFile file =
  Directory.removeFile file
