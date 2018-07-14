{-# LANGUAGE Safe #-}
module MonadIOExtras
  (
    putStrLn'
  , print'
  , putStr'
  , cowsay
  , appendFile'
  , writeFile'
  , readFile'
  , heave
  , system'
  , system''
    ) where



import Control.Monad.Trans
import System.Process
import Data.Functor
import System.Exit
import Control.Applicative

    
-- Applicable to all monadIO
putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn

putStr' :: MonadIO m => String -> m ()
putStr' = liftIO . putStr

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

writeFile' :: MonadIO m => FilePath -> String -> m ()
writeFile' fp = liftIO . writeFile fp

appendFile' :: MonadIO m => FilePath -> String -> m ()
appendFile' fp = liftIO . appendFile fp

readFile' :: MonadIO m => FilePath -> m String
readFile' = liftIO . readFile


heave :: MonadIO m => m (IO a) -> m a
heave = (>>= liftIO)

cowsay :: MonadIO m => String -> m ()
cowsay x = liftIO $ void $ system ("/usr/games/xcowsay --image=/home/shane/Pictures/characters/butler.png '" ++ x ++ "'")


system' s = liftIO (system s) >>= (\x -> case x of ExitSuccess -> pure (); _ -> empty)

system'' i s  = do (e, o, _) <- liftIO $ readCreateProcessWithExitCode ( shell s ) i
                   case e of
                     ExitSuccess -> return o
                     _ -> empty
