module StandardPrompt
  (
    standardPrompt
  , promptLine
  ) where

import Query
import System.Console.ANSI
import System.IO

standardPrompt :: IO Query
standardPrompt = promptLine "> "


promptLine p = do
    fg Vivid Red
    putStr p
    fg Vivid Green
    hFlush stdout
    l <- getLine
    fg Dull White
    hFlush stdout
    return (query l)


fg v c = setSGR [SetColor Foreground v c]


bg v c = setSGR [SetColor Background v c]

