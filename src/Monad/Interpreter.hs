{-# LANGUAGE Trustworthy, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Monad.Interpreter
  (
  -- * The Interpreter monad
    Interpreter
  , Interpreter'
  , toInterpreter
  , toInterpreter'
  , interpreterToInterpreter' 
  , Env (..)
  , InterState (..)
  , Message (..)
  , initialState
  , runInterpreter
  , runInterpreter'
  , unInterpreter
  , unInterpreter'

  , liftY
  , countFailures
  , setStateZero
  ) where

import Internal
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Applicative
import System.Random
import Control.Monad.State.Strict


data Env c = Env {
    dataFiles :: FilePath
  , theprompt :: IO Query
  , environment :: c -- ^ Any additional environment that the user may wish to use
  }


data Message = NoMessage | EscapeMode | Interrupted



-- NOTE: If the interpreter itself carries information about the new interpreter, the interpreter will stop being a monad!
-- | 
data InterState b = InterState {
    interState :: Int
  , variety :: StdGen -- ^ This seed may be used to generate random responses
  , thequery :: Query -- ^ The current query to be processed
  , localquery :: Query -- ^ A place to stash any sub-queries that will not corrupt the original query while various options are being tried out
  , lastquery :: Query
  , memory :: b -- ^ Any additional state that the user may wish to use
  , internalMessage :: Message
  }


initialState :: InterState b
initialState = InterState 0 (mkStdGen 0) (query "") (query "") (query "") undefined NoMessage




-- The Interpreter and Interpreter' monads

-- Need to convert Interpreter into a mode because if the interpreter itself carries information about the new interpreter, it will stop being a monad!
newtype Interpreter c b a = Interpreter {unInterpreter :: MaybeT (StateT (InterState b) (ReaderT (Env c) IO)) a}
  deriving (Functor, Monad, Applicative, Alternative, MonadIO, MonadState (InterState b), MonadReader (Env c), MonadPlus)

toInterpreter :: MaybeT (StateT (InterState b) (ReaderT (Env c) IO)) a -> Interpreter c b a
toInterpreter = Interpreter . countFailures . preventLoop

countFailures :: Monad m => MaybeT (StateT (InterState b) m) b1 -> MaybeT (StateT (InterState b) m) b1
countFailures = mapMaybeT tst

preventLoop :: (MonadState (InterState b) m, MonadIO m, Alternative m) => m a -> m a
preventLoop m = do 
     n <- interState <$> get
     if n <= 40 then m else modify' setStateZero >> liftIO (putStrLn "Taking too long!!") >> empty




tst :: Monad m => StateT (InterState b) m (Maybe a) -> StateT (InterState b) m (Maybe a)
tst m = do
  x <- m
  case x of
    Nothing -> modify (\i -> i {interState = interState i + 1}) >> pure Nothing
    y -> modify setStateZero >> pure y

setStateZero :: InterState b -> InterState b
setStateZero x = x {interState = 0}


runInterpreter :: InterState b -> Env c -> Interpreter c b a -> IO (Maybe a, InterState b)
runInterpreter s e = flip runReaderT e . flip runStateT s . runMaybeT . unInterpreter
  
newtype Interpreter' c b a = Interpreter' {unInterpreter' :: StateT (InterState b) (ReaderT (Env c) IO) a}
  deriving (Functor, Monad, Applicative, Alternative, MonadIO, MonadState (InterState b), MonadReader (Env c), MonadPlus)


toInterpreter' :: StateT (InterState b) (ReaderT (Env c) IO) a -> Interpreter' c b a
toInterpreter' = Interpreter'

runInterpreter' :: InterState b -> Env c -> Interpreter' c b a -> IO (a, InterState b)
runInterpreter' s e = flip runReaderT e . flip runStateT s . unInterpreter'



interpreterToInterpreter' :: Interpreter c b a -> Interpreter' c b (Maybe a)
interpreterToInterpreter' = toInterpreter' . runMaybeT . unInterpreter

liftY :: MaybeT (StateT (InterState b) (ReaderT (Env c) IO)) a -> Interpreter c b a
liftY = toInterpreter

