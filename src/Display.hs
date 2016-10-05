module Display
  (
    welcomeMessage
  , endMessage
  , endGameMessage
  , playerHasWonMessage
  , gameTiedMessage
  , displayMenu
  , displayTallys
  , displayBoard
  ) where

import Colors as Colors
import Board as Board

welcomeMessage :: IO()
welcomeMessage =
  printColoredMessage "CYAN" "\nWelcome to the Tic Tac Toe Game!\n"

endMessage :: IO()
endMessage =
  printColoredMessage "YELLOW" "\nGood-bye!"

endGameMessage :: IO()
endGameMessage =
  printColoredMessage "YELLOW" "\nThanks for playing!"

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

displayMenu :: [(Int, String)] -> IO ()
displayMenu menu =
  putStrLn $ "\n" ++ concatMap (\(a,b) -> formatMenuOption a b) menu

formatMenuOption :: Int -> String -> String
formatMenuOption number option =
  (show number) ++ ". " ++ option ++ "\n"

displayTallys :: [(String, Int)] -> IO ()
displayTallys tallys
  | null tallys = printColoredMessage "GREEN" "\nNo scores to show!"
  | otherwise = printColoredMessage "GREEN" $ "\nAmount of Wins Per Marker and Total Ties:\n" ++ showTallys tallys

printColoredMessage :: String -> String -> IO ()
printColoredMessage color message =
  putStrLn $ Colors.colorString color message

showTallys :: [(String, Int)] -> String
showTallys tallys =
  concatMap (\(a,b) -> formatTally a b) tallys

formatTally :: String -> Int -> String
formatTally marker tally =
  marker ++ ": " ++ (show tally) ++ "\n"

displayBoard :: Board -> IO ()
displayBoard gameBoard =
  putStrLn $ showBoard gameBoard

showBoard :: Board -> String
showBoard gameBoard =
  "\n" ++ concatMap (\(a,b) -> formatCell a b dimension) board
  where
    board = zip boardSpots boardVals
    dimension = Board.boardDimension gameBoard
    boardVals = Board.getBoardValues gameBoard boardSpots
    boardSpots = Board.allBoardSpots dimension

formatCell :: String -> String -> Int -> String
formatCell spot value dimension
  | (rem location dimension == 0) = "\t" ++ coreCell ++ "\n\n"
  | otherwise = "\t" ++ coreCell
  where location = read spot :: Int
        coreCell = formCellDisplay spot value

formCellDisplay :: String -> String -> String
formCellDisplay spot value =
  let coloredSpot = Colors.colorString "LIGHT YELLOW" spot
  in coloredSpot ++ ": " ++ valueDisplay value

valueDisplay :: String -> String
valueDisplay [] = " "
valueDisplay value = value
