{-# LANGUAGE DeriveGeneric #-}

module Photoname.Common
  ( Artist (..)
  , ConfigPath (..)
  , CopySwitch (..)
  , DestPath (..)
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
import Control.Newtype.Generics
import GHC.Generics
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

newtype CopySwitch = CopySwitch Bool
  deriving Generic

instance Newtype CopySwitch

newtype NoDirsSwitch = NoDirsSwitch Bool
  deriving Generic

instance Newtype NoDirsSwitch

data Links = Exactly CNlink | NoLimit

newtype MoveSwitch = MoveSwitch Bool
  deriving Generic

instance Newtype MoveSwitch

newtype NoActionSwitch = NoActionSwitch Bool
  deriving Generic

instance Newtype NoActionSwitch

newtype ParentDir = ParentDir FilePath
  deriving Generic

instance Newtype ParentDir

newtype Prefix = Prefix String
  deriving Generic

instance Newtype Prefix

newtype Suffix = Suffix String
  deriving Generic

instance Newtype Suffix

data Options = Options
  { optArtist     :: Maybe Artist
  , optConfig     :: Maybe ConfigPath
  , optCopy       :: CopySwitch
  , optNoDirs     :: NoDirsSwitch
  , optLinks      :: Links
  , optMove       :: MoveSwitch
  , optNoAction   :: NoActionSwitch
  , optParentDir  :: ParentDir
  , optPrefix     :: Prefix
  , optSuffix     :: Suffix
  , optVerbosity  :: Verbosity
  , optPaths      :: [FilePath]
  }


newtype SrcPath = SrcPath FilePath
  deriving Generic

instance Newtype SrcPath

newtype DestPath = DestPath FilePath
  deriving Generic

instance Newtype DestPath


type Ph a = ReaderT Options (ExceptT String IO) a


runRename :: Options -> Ph a -> IO (Either String a)
runRename env action = runExceptT $ runReaderT action env
