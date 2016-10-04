module SpecHelper
  (
    constructBoard
  , valueFromKey
  , deleteFile
  )
where

import Control.Monad
import qualified System.Directory as Directory
import qualified Data.Map as Map

baseMap :: Map.Map String String
baseMap = Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                       , ("4", ""), ("5", ""), ("6", "")
                       , ("7", ""), ("8", ""), ("9", "")]

constructBoard :: [(String, String)] -> Map.Map String String
constructBoard [] = baseMap
constructBoard spotOverrides =
  Map.union spots baseMap
  where spots = Map.fromList spotOverrides

valueFromKey :: Map.Map String String -> String -> String
valueFromKey boardMap key =
  boardMap Map.! key

deleteFile :: String -> IO ()
deleteFile file = do
  doesExist <- Directory.doesFileExist file
  when (doesExist :: Bool) $ Directory.removeFile file
