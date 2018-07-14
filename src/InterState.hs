{-# LANGUAGE Safe, FlexibleContexts #-}
module InterState
  (
    initialState
  , Message (..)
  , InterState (..)
  , ifSubset
  , ifSub
  , ifSubs
  , recollect
  , remember
  , obtain 
  , resetMessage
  , addMessage
  , escapeMode
  , interrupted
  , varietyFrom
  , checkMessage
  ) where


import Query
import Control.Applicative
import Control.Monad.State.Strict
import qualified Data.Set as S
import Data.InterState
import System.Random


-- | Retrieve something that was stored in the memory.
recollect :: MonadState (InterState b) f => f b
recollect = obtain memory

-- | Store something in the memory
remember :: MonadState (InterState b) m => b -> m ()
remember ys = modify' (\x -> x {memory = ys})

obtain :: MonadState a f => (a -> b) -> f b
obtain f = f <$> get




addMessage :: MonadState (InterState b) m => Message -> m ()
addMessage s = modify' (\x -> x {internalMessage = s})

resetMessage :: MonadState (InterState b) m => m ()
resetMessage = addMessage NoMessage

escapeMode :: MonadState (InterState b) m => m ()
escapeMode = addMessage EscapeMode

interrupted :: MonadState (InterState b) m => m ()
interrupted = addMessage Interrupted

varietyFrom :: MonadState (InterState b) m => [a] -> m a
varietyFrom xs = do s <- get
                    let (n, g) = randomR (0, 100 :: Int) (variety s)
                    put $ s {variety = g}
                    pure (cycle xs !! n)

-- | If the words are present in the current query, run the monad otherwise return empty
ifSub :: (MonadState (InterState b) m, Alternative m, MonadPlus m) => [String] -> m b1 -> m b1
ifSub s i = mfilter (S.fromList s `S.isSubsetOf` ) (obtain (queryWordSet . thequery) ) *> i

--ifSub s i = (obtain (queryWordSet . thequery) >>= guard . (S.fromList s `S.isSubsetOf` )) *> i

-- | If the words in either sublist are present in the current query, run the monad otherwise return empty
ifSubs :: (MonadState (InterState b) m, Alternative m, MonadPlus m) => [[String]] -> m b1 -> m b1
ifSubs ss i = msum $ map (flip ifSub i) ss


-- | If the words are present in the current query, return the pure value otherwise return empty
ifSubset :: (Alternative m, MonadState (InterState b) m, MonadPlus m) => [String] -> b1 -> m b1
ifSubset x = ifSub x . pure


checkMessage m = do
  me <- obtain internalMessage
  resetMessage
  case me of
    EscapeMode -> empty
    _ -> m
    
