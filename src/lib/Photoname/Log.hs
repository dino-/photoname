module Photoname.Log
  ( logIO
  , logP
  )
  where

import Control.Monad ( unless )

import Photoname.Common ( Ph, Options (..), ask, liftIO )


logIO :: Options -> String -> IO ()
logIO opts msg = unless (optQuiet opts) $ putStrLn msg


logP :: String -> Ph ()
logP msg = do
  opts <- ask
  liftIO $ logIO opts msg
