module Menu
  (
    runMenu
  ) where

import Control.Monad

import qualified Display as Display
import qualified GameRunner as GameRunner
import qualified Input as Input
import qualified InputValidation as Validation
import qualified Score as Score

menu :: [(String, IO ())]
menu =
  [ ("Play Game", GameRunner.runGame)
  , ("Display Scores", Score.displayScores)
  , ("Quit", putStr "") ]

runMenu :: IO ()
runMenu = do
  Display.welcomeMessage
  menuLoop
  Display.endMessage

menuLoop :: IO ()
menuLoop = go
  where go = do
          Display.displayMenu menuForDisplay
          option <- menuChoiceLoop "Please choose a valid option."
          when (not ((option :: Int) == 3)) $ do runMenuOption option
                                                 go

menuForDisplay :: [(Int, String)]
menuForDisplay =
  zip [1 .. length menu] $ map fst menu

runMenuOption :: Int -> IO ()
runMenuOption option =
  snd $ menu !! (option - 1)

menuChoiceLoop :: String -> IO Int
menuChoiceLoop promptString =  go
  where go = do
          responseIO <- Input.prompt promptString
          let response = read responseIO
          if Validation.isMenuOptionValid menuForDisplay response
            then return response
            else go
