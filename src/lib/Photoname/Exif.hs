{-# LANGUAGE FlexibleContexts #-}

module Photoname.Exif
  ( getExifDate
  )
  where

import Control.Monad.Except ( MonadIO, liftIO )
import qualified Data.Map as M
import Data.Monoid ( First (..), getFirst )
import Graphics.HsExif ( ExifTag, ExifValue, dateTime, dateTimeDigitized,
  dateTimeOriginal, parseFileExif )


{-
  Load EXIF information from filepath, or evaluate to Nothing
-}

getExifDate :: MonadIO m => FilePath -> m (Maybe String)
getExifDate oldPath = liftIO $ extractDate <$> parseFileExif oldPath


{-
  Extract the date from the passed in EXIF data, if we got any from the file.
  Evaluate to Nothing if we can't locate a date at all.
-}

extractDate :: Either String (M.Map ExifTag ExifValue) -> Maybe String
extractDate (Left _) = Nothing
extractDate (Right exifMap) =
  -- Find the first date available in the Map
  show <$> ( getFirst . mconcat . map First $ map (flip M.lookup exifMap)
    [dateTimeOriginal, dateTimeDigitized, dateTime] )
