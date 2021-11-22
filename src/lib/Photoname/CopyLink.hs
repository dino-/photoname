{-# LANGUAGE ScopedTypeVariables #-}

module Photoname.CopyLink
   ( createNewLink
   )
   where

import Control.Exception ( try )
import Control.Monad ( unless, when )
import Data.Time.LocalTime ( LocalTime )
import GHC.IO.Exception ( IOException )
import System.Directory ( copyFile, createDirectoryIfMissing )
import System.FilePath ( (</>), takeDirectory )
import System.Posix ( createLink, fileExist, removeLink )
import Text.Printf ( printf )

import Photoname.Common ( Ph, Options (..), ask, asks, liftIO, throwError )
import Photoname.Date
  ( PhDate (ExifDate, FilenameDate, NoDateFound)
  , formatDateHyphens, formatDateTime, formatYear
  )
import Photoname.Log ( lname, noticeM, warningM )


createNewLink :: PhDate -> FilePath -> Ph FilePath
createNewLink imageDate oldPath = do
  opts <- ask
  newPath <- case imageDate of
    ExifDate lt -> buildDatePath lt
    FilenameDate lt -> buildDatePath lt
    NoDateFound -> throwError "Could not extract any date information"

  -- Check for existance of the target file
  exists <- liftIO $ fileExist newPath
  when exists $ throwError $ "Destination " ++ newPath ++ " exists!"

  -- Display what will be done
  liftIO $ noticeM lname $ oldPath ++ " -> " ++ newPath

  unless (optNoAction opts) $ do
    -- Make the target dir
    liftIO $ createDirectoryIfMissing True $ takeDirectory newPath

    -- Make the new file
    if (optCopy opts)
      then liftIO $ copyFile oldPath newPath
      else tryHardLink oldPath newPath

    -- If user has specified, remove the original link
    when (optMove opts) $
       liftIO $ removeLink oldPath

  return newPath


tryHardLink :: FilePath -> FilePath -> Ph ()
tryHardLink oldPath newPath = do
  ei <- liftIO $ try $ createLink oldPath newPath
  either failureHandler return ei
  where
    failureHandler :: IOException -> Ph ()
    failureHandler _ = do
      liftIO $ warningM lname "Hard link failed, attempting to copy instead"
      liftIO $ copyFile oldPath newPath


{- Given a path to a file with EXIF data, construct a new path based on the
   date and some serial number info we can parse out of the filename.
-}
buildDatePath :: LocalTime -> Ph FilePath
buildDatePath date = do
   prefix <- asks optPrefix
   suffix <- asks optSuffix
   let fileName = printf "%s%s%s.jpg" prefix (formatDateTime date) suffix

   parentDir <- asks optParentDir
   noDirs <- asks optNoDirs
   return $ if (noDirs)
      then parentDir </> fileName
      else parentDir </> (formatYear date) </>
         (formatDateHyphens date) </> fileName
