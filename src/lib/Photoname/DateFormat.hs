module Photoname.DateFormat
   ( buildDatePath
   )
   where

import Data.Time.LocalTime ( LocalTime )
import System.FilePath ( (</>) )
import Text.Printf ( printf )

import Photoname.Common ( Ph, Options (..), asks )
import Photoname.Date ( formatDateHyphens, formatDateTime, formatYear )


{- Given a path to a file with EXIF data, construct a new path based on the
   date and some serial number info we can parse out of the filename.
-}
buildDatePath :: LocalTime -> Ph FilePath
buildDatePath date = do
   suffix <- asks optSuffix
   let fileName = printf "%s%s.jpg" (formatDateTime date) suffix

   parentDir <- asks optParentDir
   noDirs <- asks optNoDirs
   return $ if (noDirs)
      then parentDir </> fileName
      else parentDir </> (formatYear date) </>
         (formatDateHyphens date) </> fileName
