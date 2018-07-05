{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}

module Response
    (
      applyResp
    , Resp (..)

    , dontknow
    , justresponse
    --, toTry
    , Try (..)
--    , futureMode 
    --, (%$)
    , ($$)

    , stateResp
    , toIO
    , modeToState
    --, applyMonad
    ) where

import Control.Applicative
import System.Process
import System.Console.ANSI
import Data.Char
import Data.List.Split (splitOn)
import Rainbow
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Maybe





newtype Try m s a = Try { applyTry :: s -> m a } deriving (Functor)


--toTry = Try

--futureMode m = Resp . applyTry . fmap (\s -> (s, m))

--(%$) = flip futureMode
--infixl 8 %$


($$) = ($)
infixr 8 $$
  
instance Applicative m => Applicative (Try m s) where
  Try f <*> Try x = Try (\s -> ($) <$> f s <*> x s)
  pure = Try . const . pure


instance Alternative m => Alternative (Try m s) where
  Try a <|> Try b = Try (\s -> a s <|> b s)
  empty = Try $ const empty

dontknow :: Alternative m => Try m b a
dontknow = Try (const empty)

justresponse s' = Try (const $ pure s')



newtype Resp m s a = Resp {appResp :: s -> m (a, Resp m s a)} deriving (Functor)

applyResp = appResp

instance Applicative m => Applicative (Resp m s) where
  Resp f <*> Resp g = Resp (\s -> test <$> f s <*> g s)
    where test (x1, g1) (x2, g2) = (x1 x2, g1 <*> g2)
      
  pure x = Resp (const (pure (x, pure x)))


instance (Alternative m, Monad m) => Alternative (Resp m s) where
  Resp f <|> Resp g = Resp (\s -> f s <|> g s)

  empty = Resp (const empty)
  

stateResp s = StateT (flip applyResp s)


toIO :: Monad m => Resp (MaybeT m) s a -> m a -> Resp (MaybeT m) s a -> Resp m s a
toIO m x a = Resp $ \s -> do
  z <- runMaybeT (applyResp a s)
  case z of
          Just (x2, f2) -> return (x2 ,toIO m x f2)
          _ -> do
            z' <- runMaybeT (applyResp m s)
            case z' of
              Just (x3, f3) -> return (x3, toIO m x f3)
              _ -> (,) <$> x <*> pure (toIO m x m)

modeToState :: Monad m => (s -> m () ) -> (q -> s -> s) -> q -> StateT s m ()
modeToState p f q = StateT (\s -> (,) <$> p s <*> pure (f q s))



--applyMonad :: Monad m => (a -> m b) -> Resp m s a -> Resp m s b
--applyMonad f r = Resp $ \s -> do (x, r') <- applyResp r s; (,) <$> f x <*> pure (applyMonad f r')
