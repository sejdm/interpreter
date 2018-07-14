{-# LANGUAGE Safe, DeriveFunctor #-}
module Data.Mode
  (
    Mode'
  , toMode
  , runMode
  , getModeValue
  , getNextMode
  , assignMode
  , toModeWith
  ) where

import Control.Applicative
import Control.Monad.Trans.State



-- Modes

-- | A mode, when run, extracts from an action, a value and a new mode to be used as the next action.
newtype Mode' m a = Mode {
  runMode :: m (a, Mode' m a) -- ^ The inverse of 'toMode'; converts a mode to the value and future mode pair.
  } deriving (Functor)

-- | Convert an @m (a, Mode' m a)@ to @Mode'@. The first argument is interpeted as the value of the mode and the second argument as the future Mode.
toMode :: m (a, Mode' m a) -> Mode' m a
toMode = Mode

-- | Assign a future Mode from within the monad so that now it can be converted to a mode using 'toMode'
assignMode :: Functor f => Mode' f a -> f a -> f (a, Mode' f a)
assignMode m x =  flip (,) m <$> x

-- | Construct a mode by assigning a future Mode to a monadic value. Less flexible than 'assignMode' which allows you to make decisions on what future mode to assign.
toModeWith :: Functor m => Mode' m a -> m a -> Mode' m a
toModeWith m = toMode . assignMode m

getModeValue :: Functor m => Mode' m a -> m a
getModeValue = fmap fst . runMode

getNextMode :: Monad m => Mode' m a -> m (Mode' m a)
getNextMode =  fmap snd . runMode

instance Applicative f => Applicative (Mode' f) where
  fm <*> xm = toMode $ k <$> f <*> x
   where f = runMode fm
         x = runMode xm
         k (f', i') (x', i'') = (f' x', i' <*> i'')

  pure x = toMode $ pure (x, pure x)

instance (Alternative m, Monad m) => Alternative (Mode' m) where
  Mode f <|> Mode x = Mode ( f <|> x)

  empty = Mode empty



{-

    do (f', i') <- f
       (x', i'') <- x
       return (f' x', i' <*> i'')
-}
