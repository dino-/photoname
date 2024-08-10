{-# LANGUAGE DuplicateRecordFields #-}

module Photoname.Common
  ( Artist (..)
  , ConfigPath (..)
  , CopySwitch (..)
  , DestPath (..)
  , Extension (..)
  , Links(..)
  , MoveSwitch (..)
  , NoActionSwitch (..)
  , NoDirsSwitch (..)
  , ParentDir (..)
  , Options (..)
  , Ph
  , Prefix (..)
  , SrcPath (..)
  , Suffix (..)
  , Verbosity (..)
  , readVerbosity
  , runRename

  -- Re-exporting:
  , MonadError
  , ask, asks
  , liftIO
  , throwError
  )
  where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans (liftIO)
import System.Log.Logger (Priority (..))
import System.Posix (CNlink)


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


newtype Artist = Artist String

newtype ConfigPath = ConfigPath FilePath

newtype CopySwitch = CopySwitch { v :: Bool }

newtype NoDirsSwitch = NoDirsSwitch { v :: Bool }

data Extension = Extension FilePath | UseExistingExtension

data Links = Exactly CNlink | NoLimit

newtype MoveSwitch = MoveSwitch { v :: Bool }

newtype NoActionSwitch = NoActionSwitch { v :: Bool }

newtype ParentDir = ParentDir { v :: FilePath }

newtype Prefix = Prefix { v :: String }

newtype Suffix = Suffix { v :: String }

data Options = Options
  { artist     :: Maybe Artist
  , config     :: Maybe ConfigPath
  , copy       :: CopySwitch
  , noDirs     :: NoDirsSwitch
  , extension  :: Extension
  , links      :: Links
  , move       :: MoveSwitch
  , noAction   :: NoActionSwitch
  , parentDir  :: ParentDir
  , prefix     :: Prefix
  , suffix     :: Suffix
  , verbosity  :: Verbosity
  , paths      :: [FilePath]
  }


newtype SrcPath = SrcPath { v :: FilePath }

newtype DestPath = DestPath FilePath


type Ph a = ReaderT Options (ExceptT String IO) a


runRename :: Options -> Ph a -> IO (Either String a)
runRename env action = runExceptT $ runReaderT action env
