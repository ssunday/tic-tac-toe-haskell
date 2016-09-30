module GameLogic
  (
    isOver
  , isTied
  , getWinningPlayer
   ) where

import Board as Board

isOver :: Board -> Bool
isOver gameBoard =
  isTied gameBoard  || isWon gameBoard

isTied :: Board -> Bool
isTied gameBoard =
  let boardVals = Board.getBoardValues gameBoard Board.allBoardSpots
  in allNotNull boardVals && (not.isWon) gameBoard

getWinningPlayer :: Board -> String
getWinningPlayer gameBoard
  | (null winningRows) = ""
  | otherwise = head (head winningRows)
  where winningRows = getWinningRows(gameBoard)

isWon :: Board -> Bool
isWon gameBoard =
  not . null $ getWinningRows gameBoard

getWinningRows :: Board -> [[String]]
getWinningRows gameBoard =
  filter rowHasBeenWon boardRowVals
  where boardRowVals = map (Board.getBoardValues gameBoard) winningCombinations

rowHasBeenWon :: [String] -> Bool
rowHasBeenWon rowVals =
  allEqual(rowVals) && allNotNull(rowVals)

winningCombinations :: [[String]]
winningCombinations = [ ["1", "2", "3"]
                      , ["4", "5", "6"]
                      , ["7", "8", "9"]
                      , ["1", "4", "7"]
                      , ["2", "5", "8"]
                      , ["3", "6", "9"]
                      , ["1", "5", "9"]
                      , ["3", "5", "7"] ]

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual xs =
  all (== head xs) xs

allNotNull :: Eq a => [[a]] -> Bool
allNotNull [] = True
allNotNull xs =
  and $ map (not . null) xs
