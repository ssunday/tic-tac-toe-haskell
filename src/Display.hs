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

import qualified Text.Printf as Print
import qualified Colors as Colors
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
displayTallys []     = printColoredMessage "GREEN" "\nNo scores to show!"
displayTallys tallys = printColoredMessage "GREEN" $ "\nScore Breakdown\n" ++ showTotalGames tallys ++ showTallys tallys

printColoredMessage :: String -> String -> IO ()
printColoredMessage color message =
  putStrLn $ Colors.colorString color message

showTotalGames :: [(String, Int)] -> String
showTotalGames tallys =
  Colors.colorString "WHITE" "\nTotal Games: " ++ (show $ totalGames tallys) ++ "\n"

showTallys :: [(String, Int)] -> String
showTallys tallys =
  "\nResult  #\t% of Games\n" ++ concatMap (\(a,b) -> formatTally a b totalRounds) tallys
  where
    totalRounds = totalGames tallys

formatTally :: String -> Int -> Int -> String
formatTally marker tally totalRounds =
  marker ++ ":\t" ++ (show tally) ++ "\t" ++ displayedPercentage ++ "%\n"
  where
    displayedPercentage = Print.printf "%.2f" percentage :: String
    percentage = ((fromIntegral tally) / (fromIntegral totalRounds) * 100.00) :: Double

totalGames :: [(String, Int)] -> Int
totalGames tallys =
  sum $ snd <$> tallys

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
  | otherwise                     = "\t" ++ coreCell
  where location = read spot :: Int
        coreCell = formCellDisplay spot value

formCellDisplay :: String -> String -> String
formCellDisplay spot value =
  let coloredSpot = Colors.colorString "LIGHT YELLOW" spot
  in coloredSpot ++ ": " ++ valueDisplay value

valueDisplay :: String -> String
valueDisplay []    = " "
valueDisplay value = value
