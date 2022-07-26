{-# LANGUAGE FlexibleContexts #-}

module Photoname.Exif
  ( getExifDate
  )
  where

import Control.Exception ( try )
import Control.Monad.Except ( MonadIO, (>=>), liftIO )
import Data.Monoid ( First (..) )
import Graphics.Exif ( Exif, fromFile, getTag )
import System.IO.Error ( ioeGetErrorString, isUserError )

import Photoname.Common ( SrcPath (..) )



{- load Exif information from a filename, returning Nothing if libexif
   encounters a NULL instead of raising an IO error.

   This is somewhat bogus because of the error type that is used by
   the EXIF library. We have to compare the error string to see if it
   is the kind of user error that we expect. If we build against an
   EXIF library that raises some other exception, then this build will
   still succeed, and the exception will be propagated instead of
   transformed into a handled photoname error.
-}
safeExif :: FilePath -> IO (Maybe Exif)
safeExif = try . fromFile >=> either handleBadExif (return . Just)
    where
      isBadExif e =
         ioeGetErrorString e == "mkExif: NULL" && isUserError e
      handleBadExif e =
         if isBadExif e then return Nothing else ioError e


{- Get shoot date from the exif information. There are several tags 
   potentially containing dates. Try them in a specific order until we
   find one that has data.
-}
getExifDate :: MonadIO m => SrcPath -> m (Maybe String)
getExifDate (SrcPath fp) = do
  -- Get EXIF data from the image file, if present
  mbExif <- liftIO $ safeExif fp

  -- These are the EXIF tags we're interested in looking for, and in this order
  let dateTagNames = ["DateTimeOriginal", "DateTimeDigitized", "DateTime"]

  -- This code is constructing an ordered list of `getTag` actions for the
  -- above tags in the EXIF data that will be performed by the First Semigroup
  -- expression below. In the event the EXIF data is missing (Nothing), we
  -- simply evaluate to an (m [])
  lSearches <- maybe (pure []) (\e -> mapM (liftIO . getTag e) dateTagNames) mbExif

  -- Now get the first one that doesn't fail
  pure . getFirst . mconcat . map First $ lSearches
