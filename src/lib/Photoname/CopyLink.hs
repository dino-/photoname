{-# LANGUAGE ScopedTypeVariables #-}

module Photoname.CopyLink
   ( createNewLink
   )
   where

import Control.Exception ( try )
import Control.Monad ( unless, when )
import Control.Newtype.Generics ( op )
import Data.Time.LocalTime ( LocalTime )
import GHC.IO.Exception ( IOException )
import System.Directory ( copyFile, createDirectoryIfMissing )
import System.FilePath ( (</>), takeDirectory )
import System.Posix ( createLink, fileExist, removeLink )
import Text.Printf ( printf )

import Photoname.Common ( CopySwitch (..), DestPath (..), MoveSwitch (..),
  NoActionSwitch (..), NoDirsSwitch (..), ParentDir (..), Options (..),
  Ph, Prefix (..), SrcPath (..), Suffix (..),
  ask, asks, liftIO, throwError )
import Photoname.Date
  ( PhDate (ExifDate, FilenameDate, NoDateFound)
  , formatDateHyphens, formatDateTime, formatYear
  )
import Photoname.Log ( lname, noticeM, warningM )


createNewLink :: PhDate -> SrcPath -> Ph DestPath
createNewLink imageDate srcPath@(SrcPath srcFp) = do
  opts <- ask
  destPath@(DestPath destFp) <- case imageDate of
    ExifDate lt -> buildDatePath lt
    FilenameDate lt -> buildDatePath lt
    NoDateFound -> throwError "Could not extract any date information"

  -- Check for existance of the target file
  exists <- liftIO $ fileExist destFp
  when exists $ throwError $ "Destination " ++ destFp ++ " exists!"

  -- Display what will be done
  liftIO $ noticeM lname $ srcFp ++ " -> " ++ destFp

  unless (op NoActionSwitch . optNoAction $ opts) $ do
    -- Make the target dir
    liftIO $ createDirectoryIfMissing True $ takeDirectory destFp

    -- Make the new file
    if (op CopySwitch . optCopy $ opts)
      then liftIO $ copyFile srcFp destFp
      else tryHardLink srcPath destPath

    -- If user has specified, remove the original link
    when (op MoveSwitch . optMove $ opts) $
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


{- Given a path to a file with EXIF data, construct a new path based on the
   date and some serial number info we can parse out of the filename.
-}
buildDatePath :: LocalTime -> Ph DestPath
buildDatePath date = do
   prefixStr <- asks (op Prefix . optPrefix)
   suffixStr <- asks (op Suffix . optSuffix)
   let fileName = printf "%s%s%s.jpg" prefixStr (formatDateTime date) suffixStr

   parentFp <- asks (op ParentDir . optParentDir)
   noDirs <- asks (op NoDirsSwitch . optNoDirs)
   pure . DestPath $ if (noDirs)
      then parentFp </> fileName
      else parentFp </> (formatYear date) </>
         (formatDateHyphens date) </> fileName
