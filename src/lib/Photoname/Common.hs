module Photoname.Common
   ( Ph, runRename

   -- Re-exporting:
   , MonadError
   , ask, asks
   , liftIO
   , throwError
   )
   where

import Control.Monad.Except
import Control.Monad.Reader

import Photoname.Opts ( Options (..) )


type Ph a = ReaderT Options (ExceptT String IO) a


runRename :: Options -> Ph a -> IO (Either String a)
runRename env action = runExceptT $ runReaderT action env
