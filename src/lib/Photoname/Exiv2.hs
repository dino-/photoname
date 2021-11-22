module Photoname.Exiv2
  ( setArtist
  , setExifDate
  )
  where

import Control.Monad ( unless )
import System.Process ( callCommand )
import Text.Printf ( printf )

import Photoname.Common ( Ph, Options (..), ask, liftIO )
import Photoname.Date ( PhDate (FilenameDate), formatDateForExif )
import Photoname.Log ( lname, noticeM )


execCommands :: [String] -> Ph ()
execCommands commands = do
  opts <- ask

  -- Display what will be done
  liftIO $ mapM_ (noticeM lname) commands

  -- Execute the commands
  unless (optNoAction opts) $ liftIO $ mapM_ callCommand commands

  return ()


setArtist :: String -> Ph ()
setArtist newPath = do
  opts <- ask

  case optArtist opts of
    Nothing -> return ()
    Just "" -> execCommands
      [ printf "exiv2 --Modify 'del Exif.Image.Artist' %s" newPath ]
    Just artistInfo -> execCommands
      [ printf "exiv2 --Modify 'set Exif.Image.Artist %s' %s" artistInfo newPath ]


setExifDate :: PhDate -> FilePath -> Ph ()

setExifDate (FilenameDate lt) newPath =
  execCommands
    [ printf "exiv2 --Modify 'set Exif.Image.DateTime Ascii %s' --Modify 'set Exif.Photo.UserComment charset=Ascii DateTime is a guess' %s" (formatDateForExif lt) newPath
    ]

setExifDate _ _ = return ()
