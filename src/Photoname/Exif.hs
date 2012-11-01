-- Copyright: 2007-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module Photoname.Exif
   where

import Control.Exception
import Control.Monad.Error
import Graphics.Exif ( fromFile, getTag, Exif )
import System.IO.Error


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
getDate :: (MonadError String m, MonadIO m) => FilePath -> m String
getDate = loadExif >=> getOneOf dateTagNames
    where
      loadExif = (liftIO . safeExif) >=>
                 maybe (throwError "Failed EXIF loading") return

      getOneOf [] _ = throwError "has no EXIF date"
      getOneOf (tagName:tagNames) exif =
         maybe (getOneOf tagNames exif) return =<<
            liftIO (getTag exif tagName)

      dateTagNames =
         ["DateTimeDigitized", "DateTimeOriginal", "DateTime"]
