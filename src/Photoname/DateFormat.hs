-- Copyright: 2007-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Photoname.DateFormat
   ( buildDatePath
   )
   where

import System.FilePath
import Text.Printf

import Photoname.Common
import Photoname.Date
import Photoname.Exif
import Photoname.Opts ( Options (..) )


{- Given a path to a file with EXIF data, construct a new path based on the
   date and some serial number info we can parse out of the filename.
-}
buildDatePath :: FilePath -> Ph FilePath
buildDatePath oldPath = do
   dateString <- getDate oldPath
   let date = readDate dateString

   suffix <- asks optSuffix
   let fileName = printf "%s%s.jpg" (formatDateTime date) suffix

   parentDir <- asks optParentDir
   noDirs <- asks optNoDirs
   return $ if (noDirs)
      then parentDir </> fileName
      else parentDir </> (formatYear date) </>
         (formatDateHyphens date) </> fileName
