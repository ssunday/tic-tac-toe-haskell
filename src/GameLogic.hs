module GameLogic
  (
    isOver
  , isTied
  , getWinningPlayer
  ) where

import Board as Board

isOver :: Board -> Bool
isOver gameBoard =
  isTied gameBoard || isWon gameBoard

isTied :: Board -> Bool
isTied gameBoard =
  let boardVals = Board.getBoardValues gameBoard boardSpots
      boardSpots = Board.allBoardSpots $ boardDimension gameBoard
  in allNotNull boardVals && (not . isWon $ gameBoard)

getWinningPlayer :: Board -> String
getWinningPlayer gameBoard
  | null winningRows = ""
  | otherwise = head . head $ winningRows
  where winningRows = getWinningRows gameBoard

isWon :: Board -> Bool
isWon gameBoard =
  not . null . getWinningRows $ gameBoard

getWinningRows :: Board -> [[String]]
getWinningRows gameBoard =
  filter rowHasBeenWon boardRowVals
  where
    boardRowVals = map (Board.getBoardValues gameBoard) winningSpots
    winningSpots = winningCombinations $ Board.boardDimension gameBoard

rowHasBeenWon :: [String] -> Bool
rowHasBeenWon rowVals =
  allEqual rowVals && allNotNull rowVals

winningCombinations :: Int -> [[String]]
winningCombinations dimension
  | dimension == 3 = winningCombinations3x3
  | otherwise = winningCombinations4x4

winningCombinations3x3 :: [[String]]
winningCombinations3x3 = [ ["1", "2", "3"]
                         , ["4", "5", "6"]
                         , ["7", "8", "9"]

                         , ["1", "4", "7"]
                         , ["2", "5", "8"]
                         , ["3", "6", "9"]

                         , ["1", "5", "9"]
                         , ["3", "5", "7"] ]

winningCombinations4x4 :: [[String]]
winningCombinations4x4 = [ ["1", "2", "3", "4"]
                         , ["5", "6", "7", "8"]
                         , ["9", "10", "11", "12"]
                         , ["13", "14", "15", "16"]

                         , ["1", "5", "9", "13"]
                         , ["2", "6", "10", "14"]
                         , ["3", "7", "11", "15"]
                         , ["4", "8", "12", "16"]

                         , ["1", "6", "11", "16"]
                         , ["4", "7", "10", "13"]]

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual xs =
  all (== head xs) xs

allNotNull :: Eq a => [[a]] -> Bool
allNotNull [] = True
allNotNull xs =
  and $ map (not . null) xs
