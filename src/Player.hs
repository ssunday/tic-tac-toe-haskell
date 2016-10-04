module Player
  (
    getPlayerMove
  ) where

import qualified Input as Input
import qualified Colors as Colors
import Board as Board

getPlayerMove :: Board -> IO String
getPlayerMove gameBoard = go gameBoard
  where go board = do
          let inputPrompt = Colors.colorString "LIGHT PURPLE" "\nPlease enter an open spot (1-9): "
          spot <- Input.prompt inputPrompt
          let moveLoc = spot :: String
          if Board.isSpotOpen board moveLoc
            then return moveLoc
            else go board
