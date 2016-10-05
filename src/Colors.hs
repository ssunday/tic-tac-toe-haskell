module Colors
  (
    colorString
  ) where

import qualified Data.Maybe as Maybe

colors :: [(String, String)]
colors = [ ("WHITE", "\x1b[0m")
         , ("GREEN", "\x1b[32m")
         , ("YELLOW", "\x1b[33m")
         , ("BLUE", "\x1b[34m")
         , ("PURPLE", "\x1b[35m")
         , ("CYAN", "\x1b[36m")
         , ("LIGHT GREY", "\x1b[37m")
         , ("LIGHT RED", "\x1b[91m")
         , ("LIGHT GREEN", "\x1b[92m")
         , ("LIGHT YELLOW", "\x1b[93m")
         , ("LIGHT BLUE", "\x1b[94m")
         , ("LIGHT PURPLE", "\x1b[95m")
         , ("LIGHT CYAN", "\x1b[96m") ]

colorString :: String -> String -> String
colorString color s =
  let startColor = getColor color
      endColor = getColor "WHITE"
  in startColor ++ s ++ endColor

getColor :: String -> String
getColor color =
  Maybe.fromMaybe "\x1b[0m" $ lookup color colors
