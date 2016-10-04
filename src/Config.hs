module Config
  (
    getArgs
  ) where

import qualified System.Environment as Env (getArgs)

getArgs :: IO ([String])
getArgs = do
  Env.getArgs
