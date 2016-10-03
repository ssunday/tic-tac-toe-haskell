module Display
  (
    welcomeMessage
  , endMessage
  , noScoresMessage
  , displayTallys
  , playerHasWonMessage
  , gameTiedMessage
  , displayBoard
  ) where

import qualified Data.Map as Map

import Colors as Colors
import Board as Board

welcomeMessage :: IO()
welcomeMessage =
  printColoredMessage "CYAN" "\nWelcome to the Tic Tac Toe Game!\n"

endMessage :: IO()
endMessage =
  printColoredMessage "YELLOW" "\nThanks for playing and good-bye!"

playerHasWonMessage :: [Char] -> IO ()
playerHasWonMessage winningPlayerMarker =
  putStrLn $ player ++ winningPlayerMarker ++ won
  where
    color = "LIGHT BLUE"
    player = Colors.colorString color "\nPlayer "
    won = Colors.colorString color " has won!"

gameTiedMessage :: IO()
gameTiedMessage = do
  printColoredMessage "LIGHT BLUE" "\nTie!"

printColoredMessage :: String -> String -> IO ()
printColoredMessage color message =
  putStrLn $ Colors.colorString color message

noScoresMessage :: IO ()
noScoresMessage =
  putStrLn $ "No scores to show! Play some games!"

displayTallys :: [(String, Int)] -> IO ()
displayTallys tallys =
  printColoredMessage "GREEN" $ "\nAmount of Wins Per Marker and Total Ties:\n" ++ showTallys tallys

showTallys :: [(String, Int)] -> String
showTallys tallys =
  concatMap (\(a,b) -> formatTally a b) tallys

formatTally :: String -> Int -> String
formatTally marker tally =
  marker ++ " : " ++ (show tally) ++ "\n"

displayBoard :: Board -> IO ()
displayBoard gameBoard =
  putStr $ showBoard gameBoard

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

border :: String
border = "\n  -------------------------------------\n"
