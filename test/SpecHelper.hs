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

baseMap3 :: Map.Map String String
baseMap3 = Map.fromList [ ("1", ""), ("2", ""), ("3", "")
                        , ("4", ""), ("5", ""), ("6", "")
                        , ("7", ""), ("8", ""), ("9", "")]

baseMap4 :: Map.Map String String
baseMap4 = Map.fromList [ ("1", ""), ("2", ""), ("3", ""), ("4", "")
                        , ("5", ""), ("6", ""), ("7", ""), ("8", "")
                        , ("9", ""), ("10", ""), ("11", ""), ("12", "")
                        , ("13", ""), ("14", ""), ("15", ""), ("16", "")]

constructBoard :: Int -> [(String, String)] -> Map.Map String String
constructBoard dimension overrides
  | dimension == 3 = mergeMaps baseMap3 overrides
  | otherwise = mergeMaps baseMap4 overrides

mergeMaps :: Map.Map String String -> [(String, String)] -> Map.Map String String
mergeMaps baseMap overrides =
  Map.union spots baseMap
  where spots = Map.fromList overrides

valueFromKey :: Map.Map String String -> String -> String
valueFromKey boardMap key =
  boardMap Map.! key

deleteFile :: String -> IO ()
deleteFile file = do
  doesExist <- Directory.doesFileExist file
  when (doesExist :: Bool) $ Directory.removeFile file
