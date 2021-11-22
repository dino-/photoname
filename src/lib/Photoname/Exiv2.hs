module Photoname.Exiv2
  ( setArtist
  , setExifDate
  )
  where

import Control.Monad ( unless )
import System.Process ( callCommand )
import Text.Printf ( printf )

import Photoname.Common ( DestPath (..), Options (..), Ph, ask, liftIO )
import Photoname.Date ( PhDate (FilenameDate), formatDateForExif )
import Photoname.Log ( lname, noticeM )


execCommands :: [String] -> Ph ()
execCommands commands = do
  opts <- ask

  -- Display what will be done
  liftIO $ mapM_ (noticeM lname) commands

  -- Execute the commands
  unless (optNoAction opts) $ liftIO $ mapM_ callCommand commands

  pure ()


setArtist :: DestPath -> Ph ()
setArtist (DestPath destFp) = do
  opts <- ask

  case optArtist opts of
    Nothing -> pure ()
    Just "" -> execCommands
      [ printf "exiv2 --Modify 'del Exif.Image.Artist' %s" destFp ]
    Just artistInfo -> execCommands
      [ printf "exiv2 --Modify 'set Exif.Image.Artist %s' %s" artistInfo destFp ]


setExifDate :: PhDate -> DestPath -> Ph ()

setExifDate (FilenameDate lt) (DestPath destFp) =
  execCommands
    [ printf "exiv2 --Modify 'set Exif.Image.DateTime Ascii %s' --Modify 'set Exif.Photo.UserComment charset=Ascii DateTime is a guess' %s" (formatDateForExif lt) destFp
    ]

setExifDate _ _ = pure ()
