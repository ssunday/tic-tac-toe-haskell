module ScorePGSpec where

import Test.Hspec

import Database.HDBC
import Database.HDBC.PostgreSQL

import qualified ScorePG as SUT

connectTest :: IO Connection
connectTest = connectPostgreSQL "host=localhost dbname=ttt_hk_test user=postgres"

clearDB :: IO ()
clearDB = do
  conn <- connectTest
  _ <- run conn "TRUNCATE TABLE scores;" []
  _ <- commit conn
  disconnect conn

parseRow :: [(String, SqlValue)] -> String
parseRow row =
  fromSql . snd . head $ row :: String

disconnectTest :: IO ()
disconnectTest = do
  conn <- connectTest
  disconnect conn

spec :: Spec
spec =  do
  describe "insertWinner" $ after_ disconnectTest $ before_ clearDB $ do
    it "successfully adds a new winner to the table" $ do
      let winningPlayer = "R"
      _ <- SUT.insertWinner winningPlayer connectTest
      conn <- connectTest
      stmt <- prepare conn "SELECT winning_player from scores LIMIT 1;"
      _ <- execute stmt []
      resultsFromDB <- fetchAllRowsAL stmt
      let result = head $ map parseRow resultsFromDB
      result `shouldBe` winningPlayer

  describe "selectWinners" $ after_ disconnectTest $ before_ clearDB $ do
    it "returns all winning player rows in sql form" $ do
      conn <- connectTest
      _ <- run conn "INSERT INTO scores (winning_player) VALUES ('X');" []
      _ <- run conn "INSERT INTO scores (winning_player) VALUES ('R');" []

      _ <- commit conn
      results <- SUT.selectWinners connectTest
      results `shouldBe` [ [("winning_player", toSql "X")]
                         , [("winning_player", toSql "R")] ]

  describe "getTallys" $ after_ disconnectTest $ before_ clearDB $ do
    it "returns all the winning markers given a sql-style return" $ do
      let values = [ [("winning_player", toSql "X")]
                   , [("winning_player", toSql "R")]
                   , [("winning_player", toSql "Z")] ]
          results = SUT.getTallys values
      results `shouldBe` ["X", "R", "Z"]

    it "returns duplicates" $ do
      let values = [ [("winning_player", toSql "X")]
                   , [("winning_player", toSql "X")]
                   , [("winning_player", toSql "TIE")] ]
          results = SUT.getTallys values
      results `shouldBe` ["X", "X", "TIE"]
