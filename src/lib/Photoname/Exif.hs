{-# LANGUAGE FlexibleContexts #-}

module Photoname.Exif
  ( getExifDate
  )
  where

import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.Map as M
import Data.Monoid (First (..))
import Graphics.HsExif (ExifTag, ExifValue, dateTime, dateTimeDigitized,
  dateTimeOriginal, parseFileExif)

import Photoname.Common (SrcPath (SrcPath))


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
  show <$>  -- Turn the (Maybe ExifValue) into a (Maybe String)
  ( getFirst  -- Remove the First wrapper
  . mconcat  -- Collapse these to the first not-Nothing
  -- Look up all of them (resulting in [Maybe ExifValue]), wrap in First data structures
  . map (First . flip M.lookup exifMap)
  -- EXIF tags we're intersted in, in the order we want them left-to-right
  $ [dateTimeOriginal, dateTimeDigitized, dateTime])
