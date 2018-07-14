module Data.InterState
  (
    initialState
  , Message (..)
  , InterState (..)
  ) where

import Query
import System.Random

data Message = NoMessage | EscapeMode | Interrupted

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

