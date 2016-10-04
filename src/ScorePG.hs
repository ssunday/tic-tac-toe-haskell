module ScorePG
  (
    dbConnection
  , insertWinner
  , getTallys
  , selectWinners
  ) where

import Database.HDBC
import Database.HDBC.PostgreSQL

dbConnection :: IO Connection
dbConnection = connectPostgreSQL "host=localhost dbname=ttt_hk user=postgres"

insertWinner :: String -> IO Connection -> IO ()
insertWinner winner connection = do
  conn <- connection
  _ <- run conn "INSERT INTO scores (winning_player) VALUES (?);" [(toSql winner)]
  commit conn
  disconnect conn

getTallys :: [[(String, SqlValue)]] -> [String]
getTallys winners =
  map parseRow winners

selectWinners :: IO Connection -> IO [[(String, SqlValue)]]
selectWinners connection = do
  conn <- connection
  stmt <- prepare conn "SELECT winning_player from scores;"
  _ <- execute stmt []
  results <- fetchAllRowsAL stmt
  return results

parseRow :: [(String, SqlValue)] -> String
parseRow row =
  fromSql . snd . head $ row :: String
