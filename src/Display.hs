module Display
  (welcomeMessage
  , endMessage
  , playerHasWonMessage
  , gameTiedMessage
  , displayBoard
  ) where

import qualified Data.Map as Map

import Colors as Colors
import Board as Board

welcomeMessage :: IO()
welcomeMessage =
  let message = "\nWelcome to the Tic Tac Toe Game!\n"
  in putStrLn $ Colors.colorString "CYAN" message

endMessage :: IO()
endMessage =
  let message = "\nThank you for playing!"
  in putStrLn $ Colors.colorString "YELLOW" message

playerHasWonMessage :: [Char] -> IO()
playerHasWonMessage winningPlayerMarker =
  putStrLn $ "\nPlayer " ++ winningPlayerMarker ++ " has won!"

gameTiedMessage :: IO()
gameTiedMessage = do
  putStrLn $ "\nTie!"

displayBoard :: Board -> IO ()
displayBoard gameBoard =
  putStr $ showBoard gameBoard

border :: String
border = "\n  -------------------------------------\n"

showBoard :: Board -> String
showBoard gameBoard =
  border ++ concatMap (\(a,b) -> formatCell a b) board
  where
    board = Map.toList gameBoard

formatCell :: String -> String -> String
formatCell spot value
  | (mod location 3 == 0) = "   " ++ coreCell ++ "  |" ++ border
  | (mod location 3 == 1) = "   |   " ++ coreCell ++ "  |"
  | otherwise = "   " ++ coreCell ++  "  |"
  where location = read spot :: Int
        coreCell = formCellDisplay spot value

formCellDisplay :: String -> String -> String
formCellDisplay spot value =
  let coloredSpot = Colors.colorString "LIGHT YELLOW" spot
  in coloredSpot ++ " : " ++ valueDisplay value

valueDisplay :: String -> String
valueDisplay [] = " "
valueDisplay value = value
