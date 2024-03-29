module Photoname.Exiv2
  ( setArtist
  , setExifDate
  )
  where

import Control.Monad ( unless )
import Control.Newtype.Generics ( op )
import System.Process ( callCommand )
import Text.Printf ( printf )

import Photoname.Common ( Artist (..), DestPath (..), NoActionSwitch (..),
  Options (..), Ph, ask, liftIO )
import Photoname.Date ( PhDate (FilenameDate), formatDateForExif )
import Photoname.Log ( lname, noticeM )


newtype Command = Command { unCommand :: String }


execCommands :: [Command] -> Ph ()
execCommands commands = do
  opts <- ask

  -- Display what will be done
  liftIO $ mapM_ (noticeM lname . unCommand) commands

  -- Execute the commands
  unless (op NoActionSwitch . optNoAction $ opts) $
    liftIO $ mapM_ (callCommand . unCommand) commands


setArtist :: DestPath -> Ph ()
setArtist (DestPath destFp) = do
  opts <- ask

  case optArtist opts of
    Nothing -> pure ()
    Just (Artist "") -> execCommands . map Command $
      [ printf "exiv2 --Modify 'del Exif.Image.Artist' %s" destFp ]
    Just (Artist artistInfo) -> execCommands . map Command $
      [ printf "exiv2 --Modify 'set Exif.Image.Artist %s' %s" artistInfo destFp ]


setExifDate :: PhDate -> DestPath -> Ph ()

setExifDate (FilenameDate lt) (DestPath destFp) =
  execCommands . map Command $
    [ printf "exiv2 --Modify 'set Exif.Image.DateTime Ascii %s' --Modify 'set Exif.Photo.UserComment charset=Ascii DateTime is a guess' %s" (formatDateForExif lt) destFp
    ]

setExifDate _ _ = pure ()
