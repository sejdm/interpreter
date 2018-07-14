module Data.Env
  (
    Env (..)
  ) where

import Query


data Env c = Env {
    dataFiles :: FilePath
  , theprompt :: IO Query
  , output :: String -> IO ()
  , environment :: c -- ^ Any additional environment that the user may wish to use
  }
