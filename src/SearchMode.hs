{-# LANGUAGE Safe, FlexibleContexts #-}
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


searchWith :: (MonadIO m, Alternative m, MonadState (InterState b) m, MonadReader (Env c) m) => SearchSettings m () -> m ((), Mode' m ())
searchWith t = runMode (toMode $ searchList t)
  where m = applyToSearched t
        xs = searchUsingList t


match wl t = all (`isInfixOf` map toLower t) $ wl 



restrict wl s' ss | all isDigit s' = [ss !! read s']
                  | otherwise = filter (match wl) ss



enumeratePrint s = (mapM_ sayIO . zipWith (\a b -> a ++ ": " ++ b) (map (show) [0..])) $ take 10 s



-- Alternative using closures
searchList :: (MonadState (InterState b) f, Alternative f, MonadIO f, MonadReader (Env c) f) => SearchSettings f () -> f ((), Mode' f ())
searchList t = (restrict <$> fmap f (obtain (queryWordList . thequery)) <*> obtain (queryOriginal . thequery) <*> pure xs)
  >>= \z -> searchWithRestricted t {searchUsingList = z}
  where m = applyToSearched t
        m' = modeAfterSuccess t
        f = alterSearchWords t
        xs = searchUsingList t

searchWithRestricted :: (MonadState (InterState b) f, Alternative f, MonadIO f, MonadReader (Env c) f) => SearchSettings f () -> f ((), Mode' f ())
searchWithRestricted t  = 
  case z of
     [x] -> assignMode m' $ void $ m x
     [] -> empty
     ys -> assignMode (toMode $ searchList t {searchUsingList = ys}) $ sayIO ("But there are many! For example, ") >> (enumeratePrint $ take 10 ys)
  where m = applyToSearched t
        m' = modeAfterSuccess t
        f = alterSearchWords t
        z = searchUsingList t



-- Alternative using the memory provided in state
searchList' :: (MonadState (InterState [[Char]]) m, Alternative m, MonadIO m, MonadReader (Env c) m) => ([Char] -> m a) -> Mode' m ()
searchList' m = keepSameMode $
  restrict <$> obtain (queryWordList . thequery) <*> obtain (queryOriginal . thequery) <*> recollect
  --fm <- ask' master
  >>= \z ->
  case z of
     --[x] -> assignMode (modeWithNewState' [] fm) $ void ( m x )
     [x] -> void ( m x ) >> escapeMode
     [] -> empty
     ys -> sayIO ("But there are many! For example, ") >> (enumeratePrint $ take 10 ys) >> remember ys
