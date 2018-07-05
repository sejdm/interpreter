{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Mode
  (
    Mode'
  , toMode
  , runMode
  , applyMonad
  , changeFutureMode
  , getModeValue
  , getNextMode
  , assignMode
  , assignMaster
  , toModeWith
  , keepSameMode
  , futureMaster
  , useNewMode
  , modeToState
  ) where

import Control.Applicative
import Control.Monad.Trans.State



-- Modes

-- | A mode extracts, from an action, a value and a new mode to be used as the next action.
newtype Mode' m a = Mode {
  runMode :: m (a, Mode' m a) -- ^ The inverse of 'toMode'; converts a mode to the value and future mode pair.
  } deriving (Functor)

getModeValue :: Functor m => Mode' m a -> m a
getModeValue = fmap fst . runMode

getNextMode :: Monad m => Mode' m a -> m (Mode' m a)
getNextMode =  fmap snd . runMode

-- | Convert an @m (a, Mode' m a)@ to @Mode'@. The first argument is interpeted as the value of the mode and the second argument as the future Mode.
toMode :: m (a, Mode' m a) -> Mode' m a
toMode = Mode

modeToState :: StateT (Mode' m a) m a
modeToState = StateT runMode

instance Monad m => Applicative (Mode' m) where
  Mode f <*> Mode x = Mode $
    do (f', i') <- f
       (x', i'') <- x
       return (f' x', i' <*> i'')

  pure x = Mode $ pure (x, pure x)

instance (Alternative m, Monad m) => Alternative (Mode' m) where
  Mode f <|> Mode x = Mode ( f <|> x)

  empty = Mode empty


applyMonad :: Monad m => (t -> m a) -> Mode' m t -> Mode' m a
applyMonad f (Mode k) = Mode $
  do
    (x, i') <- k
    x' <- f x
    return (x', applyMonad f i')


-- | Alter the future mode.
changeFutureMode :: Monad m => Mode' m a -> Mode' m a -> Mode' m a
changeFutureMode k (Mode i) = Mode $ do
  (x, _) <- i
  pure (x, k)



-- | Assign a future Mode from within the monad so that now it can be converted to a mode using 'toMode'
assignMode :: Functor f => Mode' f a -> f a -> f (a, Mode' f a)
assignMode m x =  flip (,) m  <$> x

-- | Assign the empty mode so that the function running the mode can decide what to use.
assignMaster :: (Functor f, Alternative f, Monad f) => f a -> f (a, Mode' f a)
assignMaster = assignMode empty

-- | Assign a future Mode from outside the monad. Less flexible than 'assignMode' which allows you to make decisions on what future mode to assign.
toModeWith :: Functor m => Mode' m a -> m a -> Mode' m a
toModeWith m = toMode . assignMode m

-- | Convert the monad to a mode by assigning itself as the new mode.
keepSameMode :: Functor m => m a -> Mode' m a
keepSameMode i = let x = toModeWith x i in x

-- | Equivalent to @toMode . assignMaster@.
futureMaster :: (Alternative m, Monad m) => m a -> Mode' m a
futureMaster = toMode . assignMaster

useNewMode :: Functor f => Mode' f a -> f (a, Mode' f a)
useNewMode m = assignMode m $ getModeValue m
