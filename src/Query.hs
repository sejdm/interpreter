{-# LANGUAGE Safe, FlexibleContexts #-}
module Query
  (
    module Data.Query
  , wordsAfter
  , phraseAfter
  , between
  , wordsBetween
  , phraseBetween

  , applyResponse
  , fullQuery
  , fullQueryWords
  , toResponse
  , readFull

  ) where


import Data.Query
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Reader
import System.Process
import Control.Monad.Trans
import Monad.Response
import qualified Data.Set as S

-- | Parse the entire query
fullQuery :: (Alternative f, Monad f) => Response Query f String
fullQuery = toResponse (pure . queryOriginal)

-- | Parse all the words in the query
fullQueryWords :: (Alternative f, Monad f) => Response Query f [String]
fullQueryWords = toResponse (pure . queryWordList)

-- | Parse all the words in the query but as a set
fullQueryWordSet :: (Alternative f, Monad f) => Response Query f (S.Set String)
fullQueryWordSet = toResponse (pure . queryWordSet)



-- | Obtain the words in the query after the given string.
wordsAfter :: (Alternative f, Monad f) => String -> Response Query f [String]
wordsAfter s = fullQueryWords >>= listToAlt . after s

-- | Obtain the phrase in the query after the given string.
phraseAfter :: (Alternative f, Monad f) => String -> Response Query f String
phraseAfter s = unwords <$> wordsAfter s

-- | Obtain the words in the query between the given strings.
wordsBetween :: (Alternative f, Monad f) => String -> String -> Response Query f [String]
wordsBetween x y =  fullQueryWords >>= listToAlt . between x y

-- | Obtain the phrase in the query between the given strings.
phraseBetween :: (Alternative f, Monad f) => String -> String -> Response Query f String
phraseBetween x y = unwords <$> wordsBetween x y


readFull :: (Alternative f, Monad f, Read a) => Response Query f a
readFull = fullQuery >>= maybeToAlt . readMaybe

readAfter :: (Alternative f, Monad f, Read a) => String -> Response Query f a
readAfter s = phraseAfter s >>= maybeToAlt . readMaybe

readBetween :: (Alternative f, Monad f, Read a) => String -> String -> Response Query f a
readBetween x y = phraseBetween x y >>= maybeToAlt . readMaybe

-- Convenient functions
between :: String -> String -> [String] -> [String]
between x y s | y `elem` xs = takeWhile (/=y) xs
              | otherwise = []
            where xs = after x s


after :: String -> [String] -> [String]
after x = tail' . dropWhile (/=x)
  where tail' [] = []
        tail' y = tail y


listToAlt :: Alternative f => [a] -> f [a]
listToAlt [] = empty
listToAlt xs = pure xs

maybeToAlt :: Alternative f => Maybe a -> f a
maybeToAlt Nothing = empty
maybeToAlt (Just x) = pure x
