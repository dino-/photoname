{-# LANGUAGE OverloadedRecordDot, ScopedTypeVariables #-}

module Photoname.CopyLink
   ( createNewLink
   )
   where

import Control.Exception (try)
import Control.Monad (unless, when)
import Data.Time.LocalTime (LocalTime)
import GHC.IO.Exception (IOException)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeDirectory, takeExtension)
import System.Posix (createLink, fileExist, removeLink)

import Photoname.Common (CopySwitch (v), DestPath (..), MoveSwitch (v),
  NoActionSwitch (v), NoDirsSwitch (NoDirsSwitch), ParentDir (ParentDir),
  Options (copy, move, noAction, noDirs, parentDir, prefix, suffix), Ph,
  Prefix (Prefix), SrcPath (SrcPath), Suffix (Suffix), ask, asks, liftIO,
  throwError)
import Photoname.Date (PhDate (ExifDate, FilenameDate, NoDateFound),
  formatDateHyphens, formatDateTime, formatYear)
import Photoname.Log (lname, noticeM, warningM)


createNewLink :: PhDate -> SrcPath -> Ph DestPath
createNewLink imageDate srcPath@(SrcPath srcFp) = do
  opts <- ask
  let ext = takeExtension srcFp
  destPath@(DestPath destFp) <- case imageDate of
    ExifDate lt -> buildDatePath lt ext
    FilenameDate lt -> buildDatePath lt ext
    NoDateFound -> throwError "Could not extract any date information"

  -- Check for existence of the target file
  exists <- liftIO $ fileExist destFp
  when exists $ throwError $ "Destination " ++ destFp ++ " exists!"

  -- Display what will be done
  liftIO $ noticeM lname $ srcFp ++ " -> " ++ destFp

  unless opts.noAction.v $ do
    -- Make the target dir
    liftIO $ createDirectoryIfMissing True $ takeDirectory destFp

    -- Make the new file
    if opts.copy.v
      then liftIO $ copyFile srcFp destFp
      else tryHardLink srcPath destPath

    -- If user has specified, remove the original link
    when opts.move.v $
       liftIO $ removeLink srcFp

  pure destPath


tryHardLink :: SrcPath -> DestPath -> Ph ()
tryHardLink (SrcPath srcFp) (DestPath destFp) = do
  ei <- liftIO $ try $ createLink srcFp destFp
  either failureHandler pure ei
  where
    failureHandler :: IOException -> Ph ()
    failureHandler _ = do
      liftIO $ warningM lname "Hard link failed, attempting to copy instead"
      liftIO $ copyFile srcFp destFp


{- Construct the destination file path based on the information we have (parent
   dir, subdirs wanted or not, prefix and suffix, and the date info that was
   gathered).
-}
buildDatePath :: LocalTime -> FilePath -> Ph DestPath
buildDatePath date ext = do
   (Prefix prefixStr) <- asks prefix
   (Suffix suffixStr) <- asks suffix
   let fileName = prefixStr <> formatDateTime date <> suffixStr

   (ParentDir parentDir') <- asks parentDir
   (NoDirsSwitch noDirs') <- asks noDirs
   pure . DestPath $ if noDirs'
      then parentDir' </> fileName <.> ext
      else parentDir' </> formatYear date </>
         formatDateHyphens date </> fileName <.> ext
