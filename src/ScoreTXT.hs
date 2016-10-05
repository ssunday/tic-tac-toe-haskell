module ScoreTXT
  (
    scoreFile
  , recordWinner
  , getWinners
  , getTallys
  ) where


import qualified System.IO as IO
import qualified System.Directory as Directory

scoreFile :: String
scoreFile = "winners.txt"

recordWinner :: String -> String -> IO()
recordWinner winningPlayer file =
  IO.appendFile file (winningPlayer ++ " ")

getTallys :: String -> [String]
getTallys winners =
  filter (not . null) $ words winners

getWinners :: String -> IO String
getWinners file = do
  doesExist <- Directory.doesFileExist file
  if doesExist :: Bool
    then IO.readFile file
    else return ""
