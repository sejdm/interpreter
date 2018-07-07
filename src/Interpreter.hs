{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleContexts, NoMonomorphismRestriction #-}
module Interpreter
  (
  -- * The Interpreter monad
    Interpreter
  , Interpreter'
  , runInterpreter
  , runInterpreter'
  , Env (..)
  , InterState (..)
  , initialState
  , ask
  , ask'
  , get
  , obtain
  , put
  , modify'

  -- * Rexported from Mode
  , Mode
  , runMode
  , toMode
  , execMode
  , useNewMode
  , toModeWith
  , keepSameMode
  , futureMaster
  , applyMonad

  , sayLnIO
  , putStrLn'
  , print'
  , putStr'
  , sayIO
  , promptLine
  , cowsay
  , appendFile'
  , writeFile'
  , readFile'
  , remember
  , recollect
  , toTry
  , preventLoop
  , standardPrompt
  , intprompt
  , updateQuery
  , interpreterToInterpreter' 
  , whileJust

  , varietyFrom
  , readQuery
  , askQuery
  , promptQuery
  , tryPromptQuery
  , changeFutureMode
  , assignMode
  , assignMaster
  , withNewState
  , modeWithNewState
  , withNewState'
  , modeWithNewState'
  , on
  , raise
  , escapeMode
  , interrupted
  , readLastQuery
  , updateState

  -- * Re-exported from Response
  , ($$)
  , dontknow
  , justresponse
  , Resp
  , applyResp
  , Try

  -- Others
  , system'
  , system''
  ) where

import Text.Read (readMaybe)
import Internal
import System.Process
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Applicative
import Response
import System.Random
import Mode
import Monad.Interpreter
import System.Process
import System.Exit






-- Applicable to all monadIO
putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn

putStr' :: MonadIO m => String -> m ()
putStr' = liftIO . putStr

sayIO :: MonadIO m => String -> m ()
sayIO = putStr'

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

sayLnIO :: MonadIO m => String -> m ()
sayLnIO = putStrLn'

writeFile' :: MonadIO m => FilePath -> String -> m ()
writeFile' fp = liftIO . writeFile fp

appendFile' :: MonadIO m => FilePath -> String -> m ()
appendFile' fp = liftIO . appendFile fp

readFile' :: MonadIO m => FilePath -> m String
readFile' = liftIO . readFile


-- For Interpreter/Interpreter'
-- | Retrieve something that was stored in the memory.
recollect :: MonadState (InterState b) f => f b
recollect = obtain memory

-- | Store something in the memory
remember :: MonadState (InterState b) m => b -> m ()
remember ys = modify' (\x -> x {memory = ys})

ask' :: MonadReader a f => (a -> b) -> f b
ask' f = f <$> ask

obtain :: MonadState a f => (a -> b) -> f b
obtain f = f <$> get

on :: MonadState a1 m => (a1 -> a) -> (a -> m b) -> m b
on x f = obtain x >>= f



addMessage :: MonadState (InterState b) m => Message -> m ()
addMessage s = modify' (\x -> x {internalMessage = s})

resetMessage :: MonadState (InterState b) m => m ()
resetMessage = addMessage NoMessage

escapeMode :: MonadState (InterState b) m => m ()
escapeMode = addMessage EscapeMode

interrupted :: MonadState (InterState b) m => m ()
interrupted = addMessage Interrupted


raise :: MonadIO m => m (IO a) -> m a
raise = (>>= liftIO)

varietyFrom :: [a] -> Interpreter c b a
varietyFrom xs = do s <- get
                    let (n, g) = randomR (0, 100 :: Int) (variety s)
                    put $ s {variety = g}
                    pure (cycle xs !! n)

intprompt :: (MonadReader (Env c) m, MonadIO m) => m Query
intprompt = ask' theprompt >>= liftIO

updateQuery :: (MonadState (InterState b) f, MonadIO f, MonadReader (Env c) f) => f ()
updateQuery = intprompt >>= (\y -> modify' (\x -> x {thequery = y, lastquery = thequery x}))

-- | Reads a query
readQuery :: (MonadState (InterState b) f, Read a, Alternative f) => f a
readQuery = do m <- readMaybe <$> obtain (queryOriginal . thequery)
               case m of
                 Just k -> pure k
                 Nothing -> empty


readLastQuery :: (MonadState (InterState b) f, Read a, Alternative f) => f a
readLastQuery = do m <- readMaybe <$> obtain (queryOriginal . lastquery)
                   case m of
                     Just k -> pure k
                     Nothing -> empty


askQuery :: (MonadState (InterState b) f, Read a, Alternative f, MonadReader (Env c) f, MonadIO f) => f a
askQuery = updateQuery >> readQuery

promptQuery :: (MonadState (InterState b) f, Read a, Alternative f, MonadReader (Env c) f, MonadIO f) => String -> f a
promptQuery s = sayLnIO s >> askQuery

tryPromptQuery :: (MonadState (InterState b) f, Read a, Alternative f, MonadReader (Env c) f, MonadIO f) => String -> f a
tryPromptQuery s = readQuery <|> promptQuery s


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







type Mode c b = Mode' (Interpreter c b)



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




withNewState :: b -> Interpreter c b a -> Interpreter c () a
withNewState x i = toInterpreter $ MaybeT $ StateT (\y -> do (a, s) <- f (y {memory = x}); return (a, s {memory = ()}))
  where f = runStateT $ runMaybeT $ unInterpreter i

modeWithNewState :: t -> Mode' (Interpreter c t) a -> Mode' (Interpreter c ()) a
modeWithNewState x m = toMode $ withNewState x $  do (a, m') <- y; x' <- obtain memory; return (a, modeWithNewState x' m')
 where y = runMode m


withNewState' :: b -> Interpreter c () a -> Interpreter c b a
withNewState' x i = toInterpreter $ MaybeT $ StateT (\y -> do (a, s) <- f (y {memory = ()}); return (a, s {memory = x}))
  where f = runStateT $ runMaybeT $ unInterpreter i

modeWithNewState' :: b -> Mode c () a -> Mode c b a
modeWithNewState' x m = toMode $ withNewState' x $  do (a, m') <- y; return (a, modeWithNewState' x m')
 where y = runMode m



execMode :: InterState b -> Env c -> Interpreter' c b a -> Mode c b a -> IO (a, InterState b)
execMode s e k sm = runInterpreter' s e $ runInterpreterMode sm k sm


preventLoop :: (MonadState (InterState b) m, MonadIO m) => Mode' m () -> Mode' m ()
preventLoop a = let m = runMode a in toMode $ 
  do 
     n <- interState <$> get
     if n <= 40 then m else modify' setStateZero >> ((,) <$> sayLnIO "Taking too long!!" <*> pure a )



updateState :: MonadState (InterState b) m => m ()
updateState = modify' (\i -> i {interState = interState i + 1})


toTry :: Monad m => (a1 -> MaybeT (StateT (InterState b) m) a) -> Try (MaybeT (StateT (InterState b) m)) a1 a
toTry f = Try (countFailures . f)


standardPrompt :: IO Query
standardPrompt = promptLine "> "

cowsay :: MonadIO m => String -> m ()
cowsay x = liftIO $ void $ system ("/usr/games/xcowsay --image=/home/shane/Pictures/characters/butler.png '" ++ x ++ "'")


system' s = liftIO (system s) >>= (\x -> case x of ExitSuccess -> pure (); _ -> empty)

system'' i s  = do (e, o, _) <- liftIO $ readCreateProcessWithExitCode ( shell s ) i
                   case e of
                     ExitSuccess -> return o
                     _ -> empty

-- Others
{-
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
