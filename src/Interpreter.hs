{-# LANGUAGE Safe, FlexibleContexts #-}
module Interpreter
  (
    on
  , Mode
  , execMode

 -- May be discarded 
  , promptLine
  , preventLoop
  , standardPrompt
  , updateLocalQuery
  , whileJust
  --, readQuery
  --, askQuery
  , promptLocalQuery
  --, tryPromptQuery
  --, readLastQuery
 ---------------------

  , modeWithNewState
  , modeWithNewState'
  , updateState


  -- Others

  , module Monad.Interpreter
  , module Mode
  , module Query
  , module InterState
  , module Env
  , module MonadIOExtras
  , module Control.Monad.State.Strict
  , module Control.Monad.Reader
  , module StandardPrompt
  ) where

import InterState
import Env
import MonadIOExtras
import Text.Read (readMaybe)
import Query
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Applicative
import Mode
import Monad.Interpreter
import System.Process
import StandardPrompt



-- For Interpreter/Interpreter'


type Mode c b = Mode' (Interpreter c b)


runInt ::  Mode c b a -> Interpreter c b a -> Mode c b a -> Interpreter c b a
runInt m x i = runFullMode (modify' setStateZero >> updateQuery) m x (mapMode checkMessage i)
  

execMode :: InterState b -> Env c -> Interpreter c b a -> Mode c b a -> IO (Maybe a, InterState b)
execMode s e k sm = runInterpreter s e $ runInt sm k sm




modeWithNewState :: t -> Mode' (Interpreter c t) a -> Mode' (Interpreter c ()) a
modeWithNewState x m = toMode $ withNewState x $  do
  (a, m') <- y
  x' <- obtain memory
  return (a, modeWithNewState x' m')
 where y = runMode m


modeWithNewState' :: b -> Mode c () a -> Mode c b a
modeWithNewState' x m = toMode $ withNewState' x $  do
  (a, m') <- y
  return (a, modeWithNewState' x m')
 where y = runMode m


preventLoop :: (MonadState (InterState b) m, MonadIO m, MonadReader (Env c) m) => Mode' m () -> Mode' m ()
preventLoop a = let m = runMode a in toMode $ do 
     n <- interState <$> get
     if n <= 40 then m else modify' setStateZero >> ((,) <$> sayLnIO "Taking too long!!" <*> pure a )



updateState :: MonadState (InterState b) m => m ()
updateState = modify' (\i -> i {interState = interState i + 1})

on x f = obtain x >>= applyResponse f


updateQuery :: (MonadState (InterState b) f, MonadIO f, MonadReader (Env c) f) => f ()
updateQuery = intprompt >>= (\y -> modify' (\x -> x {thequery = y, lastquery = thequery x}))


updateLocalQuery :: (MonadState (InterState b) f, MonadIO f, MonadReader (Env c) f) => f ()
updateLocalQuery = intprompt >>= (\y -> modify' (\x -> x {localquery = y}))




-- Run the interpreter as long as it is non-empty
whileJust :: [Interpreter c b a] -> Interpreter c b [a]
whileJust = toInterpreter . MaybeT . fmap Just . tillJust




tillJust :: [Interpreter c b t] -> StateT (InterState b) (ReaderT (Env c) IO) [t]
tillJust [] = pure []
tillJust (x:xs) = 
  do t <- runMaybeT $ unInterpreter x
     case t of
       Just s -> (:) <$> pure s <*> tillJust xs
       _ -> pure []












-- May be removed

askQuery :: (MonadState (InterState b) f, Read a, Alternative f, MonadReader (Env c) f, MonadIO f) => f a
askQuery = updateQuery >> readQuery

askLocalQuery :: (MonadState (InterState b) f, Read a, Alternative f, MonadReader (Env c) f, MonadIO f) => f a
askLocalQuery = updateLocalQuery >> readLocalQuery

tryPromptQuery :: (MonadState (InterState b) f, Read a, Alternative f, MonadReader (Env c) f, MonadIO f) => String -> f a
tryPromptQuery s = readQuery <|> promptQuery s

tryPromptLocalQuery :: (MonadState (InterState b) f, Read a, Alternative f, MonadReader (Env c) f, MonadIO f) => String -> f a
tryPromptLocalQuery s = readLocalQuery <|> promptLocalQuery s



promptQuery :: (MonadState (InterState b) f, Read a, Alternative f, MonadReader (Env c) f, MonadIO f) => String -> f a
promptQuery s = sayIO s >> askQuery

promptLocalQuery :: (MonadState (InterState b) f, Read a, Alternative f, MonadReader (Env c) f, MonadIO f) => String -> f a
promptLocalQuery s = sayIO s >> askLocalQuery





-- | Reads a query
readQuery :: (MonadState (InterState b) f, Read a, Alternative f) => f a
readQuery = do m <- readMaybe <$> obtain (queryOriginal . thequery)
               case m of
                 Just k -> pure k
                 Nothing -> empty

readLocalQuery :: (MonadState (InterState b) f, Read a, Alternative f) => f a
readLocalQuery = do m <- readMaybe <$> obtain (queryOriginal . localquery)
                    case m of
                      Just k -> pure k
                      Nothing -> empty

-- May be removed
readLastQuery :: (MonadState (InterState b) f, Read a, Alternative f) => f a
readLastQuery = do m <- readMaybe <$> obtain (queryOriginal . lastquery)
                   case m of
                     Just k -> pure k
                     Nothing -> empty







{-

  --obtain thequery >>=  \s -> (if S.fromList x `S.isSubsetOf` queryWordSet s then pure s' else empty)

-- Others
runResp  :: Monad m => m s -> Resp m s a -> m b
runResp m (Resp f) = (snd <$> (m >>= f)) >>= runResp m

resp'' (Resp f) = (snd <$> (fmap thequery (lift get) >>= (countFailures . f))) >>= resp''
  

resp :: Monad m => (Query -> MaybeT (StateT (InterState b) m) (a, Resp (MaybeT (StateT (InterState b) m)) Query a)) -> MaybeT (StateT (InterState b) m) b1
resp = resp'' . resp'

resp' :: Monad m => (a1 -> MaybeT (StateT (InterState b) m) (a, Resp (MaybeT (StateT (InterState b) m)) a1 a)) -> Resp (MaybeT (StateT (InterState b) m)) a1 a
resp' f = Resp (countFailures . f)

futureMode :: Mode' (Interpreter c b) a -> Try (Interpreter c b) Query a -> Mode' (Interpreter c b) a
futureMode k (Try f) = toMode $ 
  do
    x <- fmap thequery get >>= f
    return (x, k)

-}


runInterpreterMode :: Mode c b a -> Interpreter' c b a -> Mode c b a -> Interpreter' c b a
runInterpreterMode m x i = do
  modify' setStateZero >> updateQuery
  mes <- obtain internalMessage
  resetMessage
  o <- interpreterToInterpreter' $ runMode (case mes of EscapeMode -> m; _ -> i)
  q <- obtain (queryOriginal . thequery)
  if q == "exit" then runInterpreterMode m x m
    else
        case o of
            Just (_, i') -> runInterpreterMode m x i'
            Nothing -> do
                o' <- interpreterToInterpreter' $ runMode m
                case o' of
                    Just (_, i'') -> runInterpreterMode m x i''
                    _ ->  x >> runInterpreterMode m x m
{-
runInt :: Mode c b a -> Interpreter c b a -> Mode c b a -> Interpreter c b a
--runInt m x i = foreverMode (modify' setStateZero >> updateQuery >> resetMessage) (i <|> m <|> toMode (assignMode m x))
runInt m x i = do
  modify' setStateZero >> updateQuery
  mes <- obtain internalMessage
  resetMessage
  (getNextMode (case mes of EscapeMode -> m; _ -> i) >>= runInt m x ) <|> (getNextMode m >>= runInt m x) <|> (x >> runInt m x m)
-}

--runInt m x i = foreverMode (modify' setStateZero >> updateQuery >> resetMessage) (i <|> m <|> toMode (assignMode m x))

{-
execMode :: InterState b -> Env c -> Interpreter' c b a -> Mode c b a -> IO (a, InterState b)
execMode s e k sm = runInterpreter' s e $ runInterpreterMode sm k sm
-}
