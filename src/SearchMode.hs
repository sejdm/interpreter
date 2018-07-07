{-# LANGUAGE FlexibleContexts #-}
module SearchMode (searchList, searchWithRestricted, SearchSettings (..)) where
import Interpreter
import Mode
import Query
import Data.Char
import Data.List
import Control.Applicative
import Control.Monad.Trans
import Data.Functor
import Control.Monad.State.Strict

data SearchSettings f a = SearchSettings {
    modeAfterSuccess :: Mode' f a
  , searchUsingList :: [String]
  , alterSearchWords :: [String] -> [String]
  , applyToSearched :: String -> f a
  }


searchWith :: (MonadIO m, Alternative m, MonadState (InterState b) m) => SearchSettings m () -> m ((), Mode' m ())
searchWith t = do
  runMode (toMode $ searchList t)
  where m = applyToSearched t
        xs = searchUsingList t


match wl t = all (`isInfixOf` map toLower t) $ wl 



restrict wl s' ss | all isDigit s' = [ss !! read s']
                  | otherwise = filter (match wl) ss



enumeratePrint s = (mapM_ sayLnIO . zipWith (\a b -> a ++ ": " ++ b) (map (show) [0..])) $ take 10 s



-- Alternative using closures
searchList :: (MonadState (InterState b) f, Alternative f, MonadIO f) => SearchSettings f () -> f ((), Mode' f ())
searchList t = do
  z <- restrict <$> fmap f (obtain (queryWordList . thequery)) <*> obtain (queryOriginal . thequery) <*> pure xs
  searchWithRestricted t {searchUsingList = z}
  where m = applyToSearched t
        m' = modeAfterSuccess t
        f = alterSearchWords t
        xs = searchUsingList t

searchWithRestricted :: (MonadState (InterState b) f, Alternative f, MonadIO f) => SearchSettings f () -> f ((), Mode' f ())
searchWithRestricted t  = 
  case z of
     [x] -> assignMode m' $ void $ m x
     [] -> empty
     ys -> assignMode (toMode $ searchList t {searchUsingList = ys}) $ sayLnIO ("But there are many! For example, ") >> (enumeratePrint $ take 10 ys)
  where m = applyToSearched t
        m' = modeAfterSuccess t
        f = alterSearchWords t
        z = searchUsingList t



-- Alternative using the memory provided in state
searchList' :: (MonadState (InterState [[Char]]) m, Alternative m, MonadIO m) => ([Char] -> m a) -> Mode' m ()
searchList' m = keepSameMode $ do
  z <- restrict <$> obtain (queryWordList . thequery) <*> obtain (queryOriginal . thequery) <*> recollect
  --fm <- ask' master
  case z of
     --[x] -> assignMode (modeWithNewState' [] fm) $ void ( m x )
     [x] -> void ( m x ) >> escapeMode
     [] -> empty
     ys -> sayLnIO ("But there are many! For example, ") >> (enumeratePrint $ take 10 ys) >> remember ys
