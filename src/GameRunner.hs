module GameRunner
  (
    runGame
  ) where

import Control.Monad.Loops (whileM_)

import Board as Board
import Markers
import qualified AI as AI
import qualified Display as Display
import qualified GameLogic as GameLogic
import qualified Player as Player
import qualified Score as Score
import qualified Settings as Settings

runGame :: IO()
runGame = do
  Display.welcomeMessage
  whileM_ (do
            showScoresIfPossible
            Settings.playRound
          ) $ do
    playGame
  Display.endMessage

showScoresIfPossible :: IO()
showScoresIfPossible = do
  exists <- Score.doesScoreFileExist Score.scoreFile
  if (exists :: Bool)
    then displayAvailableTallys
    else Display.noScoresMessage

displayAvailableTallys :: IO()
displayAvailableTallys = do
  winners <- Score.getWinners Score.scoreFile
  let tallys = Score.getTallys winners
  Display.displayTallys tallys

playGame :: IO()
playGame = do
  playerMarker <- Settings.getPlayerMarker
  aiPlayerMarker <- Settings.getAIMarker
  let board = Board.makeBoard
  let markers = Markers { ai = aiPlayerMarker :: String, player = playerMarker :: String }
  Display.displayBoard board
  playerMove board markers

playerMove :: Board -> Markers String String  -> IO ()
playerMove gameBoard markers = do
  spotIO <- Player.getPlayerMove gameBoard
  let spot = spotIO :: String
  let newBoard = Board.markBoard gameBoard spot $ player markers
  Display.displayBoard newBoard
  if GameLogic.isOver newBoard
  then endGame newBoard
  else aiMove newBoard markers

aiMove :: Board -> Markers String String -> IO()
aiMove gameBoard markers = do
  let newBoard = makeAIMove gameBoard markers
  Display.displayBoard newBoard
  if GameLogic.isOver newBoard
  then endGame newBoard
  else playerMove newBoard markers

makeAIMove :: Board -> Markers String String -> Board
makeAIMove gameBoard markers =
  let spot = AI.getMove gameBoard markers
  in Board.markBoard gameBoard spot (ai markers)

endGame :: Board -> IO()
endGame gameBoard = do
  _ <- Score.recordWinner winningPlayer Score.scoreFile
  reportEndGameStatus winningPlayer
  where
    winningPlayer = GameLogic.getWinningPlayer gameBoard

reportEndGameStatus :: String -> IO()
reportEndGameStatus winningPlayer
  | null winningPlayer = Display.gameTiedMessage
  | otherwise = Display.playerHasWonMessage winningPlayer
