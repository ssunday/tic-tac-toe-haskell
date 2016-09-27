module Input
  (
    prompt
  ) where

import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt s = do
  putStrLn $ s
  hFlush stdout
  getLine
