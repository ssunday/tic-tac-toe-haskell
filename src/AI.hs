module AI
  (
    getMove
  ) where

import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.Map as Map

import qualified GameLogic as GameLogic (getWinningPlayer, isOver)
import Board as Board
import Markers

getMove :: Board -> Markers String String -> String
getMove gameBoard markers  =
  head . maxScore $ assignScores gameBoard markers

assignScores :: Board -> Markers String String -> Map.Map String Int
assignScores gameBoard markers =
  Map.fromList $ zip spots scores
  where spots = Board.getAvailableSpots gameBoard
        scores = getScores gameBoard markers

getScores :: Board -> Markers String String -> [Int]
getScores gameBoard markers =
  parallelMinimax gameBoard markers (ai markers) 0

parallelMinimax :: Board -> Markers String String -> String -> Int -> [Int]
parallelMinimax gameBoard markers currentPlayer depth =
  parMap rdeepseq applyMinMax nextBoardStates
  where
    nextPlayer = getOtherPlayer markers currentPlayer
    nextBoardStates = getNextBoardStates gameBoard currentPlayer
    applyMinMax boardState = minimax boardState markers nextPlayer (depth + 1)

minimax :: Board -> Markers String String -> String -> Int -> Int
minimax gameBoard markers currentPlayer depth
  | isConditionToScoreBoard gameBoard depth = getScore gameBoard markers depth
  | otherwise =
      getMaxOrMin scores markers currentPlayer
      where scores = parallelMinimax gameBoard markers currentPlayer depth

getScore :: Board -> Markers String String -> Int -> Int
getScore gameBoard markers depth
  | winningPlayer == (ai markers) = 100 - depth
  | winningPlayer == (player markers) = depth - 100
  | otherwise = 0
  where winningPlayer = GameLogic.getWinningPlayer gameBoard

getMaxOrMin :: [Int] ->  Markers String String -> String -> Int
getMaxOrMin scores markers currentPlayerMarker
  | currentPlayerMarker == (ai markers) = maximum scores
  | otherwise = minimum scores

getOtherPlayer :: Markers String String -> String -> String
getOtherPlayer markers currentPlayer
  | currentPlayer == (ai markers) = (player markers)
  | otherwise = (ai markers)

getNextBoardStates :: Board -> String -> [Board]
getNextBoardStates gameBoard currentPlayer =
  map markSpot availableSpots
  where markSpot spot = Board.markBoard gameBoard spot currentPlayer
        availableSpots = Board.getAvailableSpots gameBoard

isConditionToScoreBoard :: Board -> Int -> Bool
isConditionToScoreBoard board depth =
  GameLogic.isOver board || depth > (maxDepth board)

maxDepth :: Board -> Int
maxDepth board
 | Board.boardDimension board > 3 = 4
 | otherwise = 10

maxScore :: Map.Map String Int -> [String]
maxScore scoredBoard = go [] Nothing (Map.toList scoredBoard)
  where
    go ks _        []           = ks
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
      | v < u     = go ks     (Just u) rest
      | v > u     = go [k]    (Just v) rest
      | otherwise = go (k:ks) (Just v) rest
