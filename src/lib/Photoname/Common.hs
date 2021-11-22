module Photoname.Common
  ( Options (..)
  , Verbosity (..)
  , Ph
  , readVerbosity
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
import System.Log.Logger ( Priority (..) )


data Verbosity
  = Quiet
  | Verbose Priority

instance Show Verbosity where
  show Quiet = "0"
  show (Verbose NOTICE) = "1"
  show (Verbose INFO) = "2"
  show (Verbose DEBUG) = "3"
  show _ = "Should never see this, invalid verbosity level being shown"

readVerbosity :: String -> Either String Verbosity
readVerbosity "0" = Right   Quiet
readVerbosity "1" = Right $ Verbose NOTICE
readVerbosity "2" = Right $ Verbose INFO
readVerbosity "3" = Right $ Verbose DEBUG
readVerbosity _   = Left    "Invalid verbosity level, expecting 0-3"


data Options = Options
  { optArtist :: Maybe String
  , optConfig :: Maybe FilePath
  , optCopy :: Bool
  , optNoDirs :: Bool
  , optMove :: Bool
  , optNoAction :: Bool
  , optParentDir :: FilePath
  , optPrefix :: String
  , optSuffix :: String
  , optVerbosity :: Verbosity
  , optPaths :: [FilePath]
  }


type Ph a = ReaderT Options (ExceptT String IO) a


runRename :: Options -> Ph a -> IO (Either String a)
runRename env action = runExceptT $ runReaderT action env
