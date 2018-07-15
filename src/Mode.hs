{-# LANGUAGE Safe #-}
module Mode
  (
    module Data.Mode
  , applyMonad
  , changeFutureMode
  , assignMaster
  , keepSameMode
  , futureMaster
  , useNewMode
  , modeToState
  , foreverMode
  , runFullMode
  , mapMode
  , changeMonadWith
  ) where

import Control.Applicative
import Control.Monad.Trans.State
import Data.Mode



modeToState :: StateT (Mode' m a) m a
modeToState = StateT runMode


applyMonad :: Monad m => (t -> m a) -> Mode' m t -> Mode' m a
applyMonad f k = toMode $
  runMode k >>= \(y, j) -> (,) <$> f y <*> pure (applyMonad f j)


-- | Alter the future mode.
changeFutureMode :: Monad m => Mode' m a -> Mode' m a -> Mode' m a
changeFutureMode k = toMode . assignMode k . getModeValue


-- | Assign the empty mode so that the function running the mode can decide what to use.
assignMaster :: (Functor f, Alternative f, Monad f) => f a -> f (a, Mode' f a)
assignMaster = assignMode empty

-- | Convert the monad to a mode by assigning itself as the new mode.
keepSameMode :: Functor m => m a -> Mode' m a
keepSameMode i = let x = toModeWith x i in x

-- | Equivalent to @toMode . assignMaster@.
futureMaster :: (Alternative m, Monad m) => m a -> Mode' m a
futureMaster = toMode . assignMaster

useNewMode :: Functor f => Mode' f a -> f (a, Mode' f a)
useNewMode m = assignMode m $ getModeValue m

foreverMode :: Monad m => m b -> Mode' m a -> m a
foreverMode mb x = (mb >> runMode x) >>= foreverMode mb . snd

-- | Run the mode but with some monad to be run before each step and using a "master" mode to fall back upon in case the alternative is empty and finally a default action in case even the master mode is empty
runFullMode :: (Monad m, Alternative m) => m b -> Mode' m a -> m a -> Mode' m a -> m a
runFullMode ma m x i = ma >> ((getNextMode i >>= f) <|> (getNextMode m >>= f) <|> (x >> f m))
  where f = runFullMode ma m x


mapMode :: (m (a, Mode' m a) -> m (b, Mode' m b)) -> Mode' m a -> Mode' m b
mapMode f = toMode . f . runMode


changeMonadWith  :: Monad m1 => (m2 (a1, Mode' m2 a1) -> m1 (a2, Mode' m2 a1)) -> Mode' m2 a1 -> Mode' m1 a2
changeMonadWith f m = toMode $ g <$> f (runMode m)
  where g (x, m') = (x, changeMonadWith f m')
  
  


    --(x, i') <- k
    --(,) <$> f x <*> pure (applyMonad f i')
    --(,) <$> f x <*> pure (applyMonad f i')
    --x' <- f x
    --return (x', applyMonad f i')
{-
changeFutureMode k (Mode i) = Mode $ do
  (x, _) <- i
  pure (x, k)
-}
