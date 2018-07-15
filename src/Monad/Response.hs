{-# LANGUAGE Trustworthy, DeriveFunctor, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving #-}
-- UndecidableInstances was needed for the same reason as in MonadReader.

module Monad.Response
  (
    applyResponse
  , toResponse
  , Response
  , monadToResponse
  ) where

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State.Strict


newtype Response q m a = Response {runResponse :: ReaderT q m a} deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus)
applyResponse = runReaderT . runResponse 

toResponse = Response . ReaderT

monadToResponse = toResponse . const

instance MonadReader r m => MonadReader r (Response q m) where
  ask = Response $ ReaderT $ const ask

  local f m = Response $ ReaderT $ local f . g
    where g = runReaderT $ runResponse m

instance MonadState r m => MonadState r (Response q m) where
  get = Response $ ReaderT $ const get
  put = Response . ReaderT . const . put
