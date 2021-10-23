module Photoname.Common
   ( Options (..)
   , Ph
   , runRename

   -- Re-exporting:
   , MonadError
   , ask, asks
   , liftIO
   , throwError
   )
   where

import Control.Monad.Except ( ExceptT, MonadError, runExceptT, throwError )
import Control.Monad.Reader ( ReaderT, ask, asks, runReaderT )
import Control.Monad.Trans ( liftIO )


data Options = Options
  { optConfig :: Maybe FilePath
  , optNoDirs :: Bool
  , optMove :: Bool
  , optNoAction :: Bool
  , optParentDir :: FilePath
  , optQuiet :: Bool
  , optSuffix :: String
  , optPaths :: [FilePath]
  }


type Ph a = ReaderT Options (ExceptT String IO) a


runRename :: Options -> Ph a -> IO (Either String a)
runRename env action = runExceptT $ runReaderT action env
