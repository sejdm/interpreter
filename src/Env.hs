{-# LANGUAGE Safe, FlexibleContexts #-}
module Env
  (
    module Data.Env
  , intprompt
  , pathForData
  , checkInEnvironment
  , pathForData'
  , checkInEnvironment'
  , sayIO
  , sayLnIO
  ) where

import Query
import Control.Monad.Reader
import Data.Env


intprompt :: (MonadReader (Env c) m, MonadIO m) => m Query
intprompt = asks theprompt >>= liftIO


pathForData :: (MonadReader (Env c) m, MonadIO m) => m FilePath
pathForData = asks dataFiles


checkInEnvironment :: (MonadReader (Env c) m, MonadIO m) => (c -> b) -> m b
checkInEnvironment f = asks (f . environment)

pathForData' :: (MonadReader (Env c) m, MonadIO m) => ReaderT q m FilePath
pathForData' = ReaderT (const (asks dataFiles))

checkInEnvironment' :: (MonadReader (Env c) m, MonadIO m) => (c -> b) -> ReaderT q m b
checkInEnvironment' f = ReaderT $ const $ asks (f . environment)

sayIO :: (MonadReader (Env c) m, MonadIO m) => String -> m ()
sayIO s = do
  f <- asks output
  liftIO $ f s


sayLnIO :: (MonadReader (Env c) m, MonadIO m) => String -> m ()
sayLnIO s = do
  f <- asks output
  liftIO $ f (s ++ "\n")
