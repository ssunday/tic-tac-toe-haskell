module GameLogic where

import qualified Data.Map as Map

board =
  Map.fromList [ ("1", "")
               , ("2", "")
               , ("3", "")
               , ("4", "")
               , ("5", "")
               , ("6", "")
               , ("7", "")
               , ("8", "")
               , ("9", "")]

markBoard gameBoard spot marker =
  let f _ = (Just marker)
  in Map.alter f spot gameBoard
