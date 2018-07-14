module Data.Query
  (
    Query
  , query
  , queryOriginal
  , queryWordList
  , queryWordSet
  ) where

import qualified Data.Set as S
import Data.List.Split
import Data.Char

-- | An abstract data type constructed using 'query'.
data Query = Query {
    queryOriginal :: String -- ^ Extract the original query
  , queryWordList :: [String] -- ^ Extract the words from the query as a list
  , queryWordSet :: S.Set String -- ^ Extract the words from the query as a set
  } deriving (Show)


-- | Construct a Query from a String
query x = Query x w (S.fromList w)
  where w = splitOneOf ".?,; " $ map toLower x


--type Response s m a = Response s m a


-- Internally, Response is just ReaderT but the following function ensures
-- that it cannot be constructed without ensuring that m is a monad and
-- an alternative

{-
toResponse :: (Alternative m, Monad m) => (s -> m a) -> Response s m a
toResponse = toResponse


applyResponse :: (Alternative m, Monad m) => Response s m a -> (s -> m a)
applyResponse = applyResponse
-}
