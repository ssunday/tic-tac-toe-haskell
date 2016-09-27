module Board
  (Board
  , makeBoard
  , allBoardSpots
  , markBoard
  , isSpotOpen
  , getAvailableSpots
  , getBoardValues
  ) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe(fromMaybe)

type Board = Map.Map String String

allBoardSpots :: [String]
allBoardSpots = map show [1 :: Int .. 9]

makeBoard :: Board
makeBoard =
  let pair number = (number, "")
      boardMap = map pair allBoardSpots
  in Map.fromList boardMap

markBoard :: Board -> String -> String -> Board
markBoard gameBoard spot marker =
  Map.alter f spot gameBoard
  where f _  = (Just marker)

isSpotOpen :: Board -> String -> Bool
isSpotOpen gameBoard spot =
  (null (getSpotValue gameBoard spot)) && (elem spot allBoardSpots)

getBoardValues :: Board -> [String] -> [String]
getBoardValues gameBoard boardSpots =
  map (Board.getSpotValue gameBoard) boardSpots

getAvailableSpots :: Board -> [String]
getAvailableSpots gameBoard =
  Map.keys $ Map.filter null gameBoard

getSpotValue :: Board -> String -> String
getSpotValue gameBoard spot =
  Maybe.fromMaybe "" $ Map.lookup spot gameBoard
