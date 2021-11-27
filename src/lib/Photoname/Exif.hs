{-# LANGUAGE FlexibleContexts #-}

module Photoname.Exif
  ( getExifDate
  )
  where

import Control.Monad.Except ( MonadIO, liftIO )
import Control.Newtype.Generics ( ala )
import qualified Data.Map as M
import Data.Monoid ( First (..) )
import Graphics.HsExif ( ExifTag, ExifValue, dateTime, dateTimeDigitized,
  dateTimeOriginal, parseFileExif )

import Photoname.Common ( SrcPath (..) )


{-
  Load EXIF information from filepath, or evaluate to Nothing
-}

getExifDate :: MonadIO m => SrcPath -> m (Maybe String)
getExifDate (SrcPath fp) = liftIO $ extractDate <$> parseFileExif fp


{-
  Extract the date from the passed in EXIF data, if we got any from the file.
  Evaluate to Nothing if we can't locate a date at all.
-}

extractDate :: Either String (M.Map ExifTag ExifValue) -> Maybe String
extractDate (Left _) = Nothing
extractDate (Right exifMap) =
  -- Find the first date available in the Map
  show <$> ala First foldMap (map (flip M.lookup exifMap)
    [dateTimeOriginal, dateTimeDigitized, dateTime])
