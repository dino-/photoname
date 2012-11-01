-- Copyright: 2007-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Photoname.Common
   ( Ph, runRename

   -- Re-exporting:
   , MonadError
   , ask, asks
   , liftIO
   , throwError
   )
   where

import Control.Monad.Error
import Control.Monad.Reader

import Photoname.Opts ( Options (..) )


type Ph a = ReaderT Options (ErrorT String IO) a


runRename :: Options -> Ph a -> IO (Either String a)
runRename env action = runErrorT $ runReaderT action env
