module ScoreTXT
  (
    scoreFile
  , recordWinner
  , getWinners
  , getTallys
  ) where


import qualified Data.List.Split as Split
import qualified System.IO as IO
import qualified System.Directory as Directory

scoreFile :: String
scoreFile = "winners.txt"

recordWinner :: String -> String -> IO()
recordWinner winningPlayer file =
  IO.appendFile file (winningPlayer ++ ",")

getTallys :: String -> [String]
getTallys commaString =
  filter (not . null) splitTallys
  where
    splitTallys = Split.splitOn "," commaString

getWinners :: String -> IO String
getWinners file = do
  doesExist <- Directory.doesFileExist file
  if doesExist :: Bool
    then IO.readFile file
    else return ""
