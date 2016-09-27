module GameRunner
  (
    runGame
  ) where

import qualified GameLogic as GameLogic
import Board as Board
import qualified AI as AI
import qualified Player as Player
import qualified Display as Display

import Settings as Settings
import Markers

runGame :: IO()
runGame = do
  Display.welcomeMessage
  playerMarker <- Settings.getPlayerMarker
  aiPlayerMarker <- Settings.getAIMarker playerMarker
  let board = Board.makeBoard
  let markers = Markers { ai = aiPlayerMarker :: String, player = playerMarker :: String }
  Display.displayBoard board
  playerMove board markers
  Display.endMessage

playerMove :: Board -> Markers String String  -> IO ()
playerMove gameBoard markers = do
  spotIO <- Player.getPlayerMove gameBoard
  let spot = spotIO :: String
  let newBoard = Board.markBoard gameBoard spot $ player markers
  Display.displayBoard newBoard
  if GameLogic.isOver newBoard
  then reportEndGameStatus newBoard markers
  else aiMove newBoard markers

aiMove :: Board -> Markers String String -> IO()
aiMove gameBoard markers = do
  let newBoard = makeAIMove gameBoard markers
  Display.displayBoard newBoard
  if GameLogic.isOver newBoard
  then reportEndGameStatus newBoard markers
  else playerMove newBoard markers

makeAIMove :: Board -> Markers String String -> Board
makeAIMove gameBoard markers =
  let spot = AI.getMove gameBoard markers
  in Board.markBoard gameBoard spot (ai markers)

reportEndGameStatus :: Board -> Markers String String -> IO()
reportEndGameStatus gameBoard markers
  | (winningPlayer == aiMarker) = Display.playerHasWonMessage aiMarker
  | (winningPlayer == playerMarker) = Display.playerHasWonMessage playerMarker
  | otherwise = Display.gameTiedMessage
  where
    aiMarker = ai markers
    playerMarker = player markers
    winningPlayer = GameLogic.getWinningPlayer gameBoard
