module Board
  ( Board
  , makeBoard
  , allBoardSpots
  , boardDimension
  , markBoard
  , isSpotOpen
  , getAvailableSpots
  , getBoardValues
  ) where

import qualified Data.Map as Map

type Board = Map.Map String String

allBoardSpots :: Int -> [String]
allBoardSpots dimension =
  show <$> [1 :: Int .. (dimension * dimension)]

boardDimension :: Board -> Int
boardDimension board =
  floor . (sqrt :: Double -> Double) . fromIntegral $ length board

makeBoard :: Int -> Board
makeBoard dimension =
  Map.fromList $ zip (allBoardSpots dimension) (repeat "")

markBoard :: Board -> String -> String -> Board
markBoard gameBoard spot marker =
  Map.alter f spot gameBoard
  where f _  = (Just marker)

isSpotOpen :: Board -> String -> Bool
isSpotOpen gameBoard spot =
  Map.member spot gameBoard && null (getSpotValue gameBoard spot)

getBoardValues :: Board -> [String] -> [String]
getBoardValues gameBoard boardSpots =
  Board.getSpotValue gameBoard <$> boardSpots

getAvailableSpots :: Board -> [String]
getAvailableSpots gameBoard =
  Map.keys $ Map.filter null gameBoard

getSpotValue :: Board -> String -> String
getSpotValue gameBoard spot =
  gameBoard Map.! spot
