module Internal
  (
    promptLine
  , Query (..)
  , query
  ) where

import qualified Data.Set as S
import System.Console.ANSI
import System.IO
import System.Process
import Control.Monad.Trans
import Data.List.Split
import Data.Char

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


data Query = Query {
    queryOriginal :: String -- ^ Extract the original query
  , queryWordList :: [String] -- ^ Extract the words from the query as a list
  , queryWordSet :: S.Set String -- ^ Extract the words from the query as a set
  } deriving (Show)

-- | Construct a Query from a String
query x = Query x w (S.fromList w)
  where w = splitOneOf ".?,; " $ map toLower x
