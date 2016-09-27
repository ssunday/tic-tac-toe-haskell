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
          let inputPrompt = Colors.colorString "LIGHT PURPLE" "\nPlease input open spot: "
          spot <- I.prompt inputPrompt
          let moveLoc = spot :: String
          if Board.isSpotOpen board moveLoc
            then return moveLoc
            else go board
