{-# LANGUAGE FlexibleContexts #-}
module Query
  (
    Query
  , query

  , queryOriginal
  , queryWordList
  , queryWordSet
  , wordsAfter
  , phraseAfter
  , between
  , wordsBetween
  , phraseBetween

  , ifSubset
  , ifSub
  , ifSubs
  ) where

import qualified Data.Set as S
import Control.Applicative
import Interpreter
import Internal
import Control.Monad.State.Strict

-- | If the words are present in the current query, run the monad otherwise return empty
ifSub :: (MonadState (InterState b) m, Alternative m, MonadPlus m) => [String] -> m b1 -> m b1
ifSub s i = mfilter (S.fromList s `S.isSubsetOf` ) (obtain (queryWordSet . thequery) ) *> i
--ifSub s i = (obtain (queryWordSet . thequery) >>= guard . (S.fromList s `S.isSubsetOf` )) *> i

-- | If the words in either sublist are present in the current query, run the monad otherwise return empty
ifSubs :: (MonadState (InterState b) m, Alternative m, MonadPlus m) => [[String]] -> m b1 -> m b1
ifSubs ss i = msum $ map (flip ifSub i) ss


-- | If the words are present in the current query, return the pure value otherwise return empty
ifSubset :: (Alternative m, MonadState (InterState b) m) => [String] -> b1 -> m b1
ifSubset x s' = obtain thequery >>=  \s -> (if S.fromList x `S.isSubsetOf` queryWordSet s then pure s' else empty)


between :: String -> String -> Query -> [String]
between x y q | y `elem` xs = takeWhile (/=y) xs
              | otherwise = []
            where xs = after x q
  
after :: String -> Query -> [String]
after x = tail' . dropWhile (/=x) . queryWordList
  where tail' [] = []
        tail' y = tail y

listToAlt :: Alternative f => [a] -> f [a]
listToAlt [] = empty
listToAlt xs = pure xs


-- | Obtain the words in the query after the given string.
wordsAfter :: Alternative f => String -> Query -> f [String]
wordsAfter s = listToAlt . after s

-- | Obtain the phrase in the query after the given string.
phraseAfter :: Alternative f => String -> Query -> f String
phraseAfter s q = unwords <$> wordsAfter s q


-- | Obtain the words in the query between the given strings.
wordsBetween :: Alternative f => String -> String -> Query -> f [String]
wordsBetween x y = listToAlt . between x y

-- | Obtain the phrase in the query between the given strings.
phraseBetween :: Alternative f => String -> String -> Query -> f String
phraseBetween x y q = unwords <$> wordsBetween x y q
