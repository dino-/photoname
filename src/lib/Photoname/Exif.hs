{-# LANGUAGE FlexibleContexts #-}

module Photoname.Exif
  ( getExifDate
  )
  where

import Control.Monad.Except ( MonadError, MonadIO, (>=>), liftIO, throwError )
import qualified Data.Map as M
import Data.Monoid ( First (..), getFirst )
import Graphics.HsExif ( ExifTag, ExifValue, dateTime, dateTimeDigitized,
  dateTimeOriginal, parseFileExif )


{-
  Load Exif information from a filepath, or throw an error in MonadError
-}

getExifDate :: (MonadError String m, MonadIO m) => FilePath -> m String
getExifDate = liftIO . parseFileExif >=> either throwError return . extractDate


{-
  Extract the date from the passed in EXIF data, if we got any from the file.
  Return a meaningful error message in Left if we can't locate a date at all.
-}

extractDate :: Either String (M.Map ExifTag ExifValue) -> Either String String
extractDate eitherExifMap = do
  -- Pull the EXIF Map out of the Either value passed in
  exifMap <- eitherExifMap

  -- Find the first date available in the Map
  let mbDateValue = getFirst . mconcat . map First $ map (flip M.lookup exifMap)
         [dateTimeOriginal, dateTimeDigitized, dateTime]

  -- Return the Either value expected by the caller, marking up failure with a
  -- meaningful error message
  maybe (Left "No dates found in EXIF data") (Right . show) mbDateValue
