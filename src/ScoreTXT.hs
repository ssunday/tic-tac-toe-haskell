module ScoreTXT
  (
    scoreFile
  , getWinners
  , recordWinner
  , getTallys
  ) where


import qualified Data.List.Split as Split
import qualified System.IO as IO
import qualified System.Directory as Directory

scoreFile :: String
scoreFile = "winners.txt"

doesScoreFileExist :: String -> IO Bool
doesScoreFileExist file =
  Directory.doesFileExist file

recordWinner :: String -> String -> IO()
recordWinner winningPlayer file =
  IO.appendFile file (winningPlayer ++ ",")

getTallys :: String -> [String]
getTallys winners =
  splitByComma winners

getWinners :: String -> IO String
getWinners file = do
  doesExist <- doesScoreFileExist file
  if (doesExist :: Bool)
    then
    IO.readFile file
    else
    return ""

splitByComma :: String -> [String]
splitByComma commaString =
  filter (not . null) split
  where
    split = Split.splitOn "," commaString
