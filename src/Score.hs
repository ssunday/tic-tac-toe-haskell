module Score
  (
    displayScores
  , tallyWinners
  , recordWinner
  , markerToRecord
  ) where

import qualified Data.List as List (nub)

import qualified Config as Config
import qualified Display as Display
import qualified ScorePG as PG
import qualified ScoreTXT as TXT

displayScores :: IO ()
displayScores = do
  isPostgres <- isPG
  if isPostgres :: Bool
    then displayPGTallys
    else displayTXTTallys

recordWinner :: String -> IO()
recordWinner winner = do
  isPostgres <- isPG
  let marker = markerToRecord winner
  if isPostgres :: Bool
    then PG.insertWinner marker PG.dbConnection
    else TXT.recordWinner marker TXT.scoreFile

isPG :: IO Bool
isPG = do
  args <- Config.getArgs
  return $ elem "pg" (args :: [String])

markerToRecord :: String -> String
markerToRecord [] = "TIE"
markerToRecord marker = marker

displayPGTallys :: IO()
displayPGTallys = do
  rows <- PG.selectWinners PG.dbConnection
  let winners = PG.getTallys rows
  let tallys = tallyWinners winners
  Display.displayTallys tallys

displayTXTTallys :: IO()
displayTXTTallys = do
  fileData <- TXT.getWinners TXT.scoreFile
  let winners = TXT.getTallys fileData
  let tallys = tallyWinners winners
  Display.displayTallys tallys

tallyWinners :: [String] -> [(String, Int)]
tallyWinners winningMarkers =
  zip uniqueMarkers occurrences
  where
    uniqueMarkers = List.nub winningMarkers
    occurrences = map numberOfMarkers uniqueMarkers
    numberOfMarkers marker = length $ filter (== marker) winningMarkers
