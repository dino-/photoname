module Photoname.Exiv2
  ( setArtist
  )
  where

import Control.Monad ( unless )
import System.Process ( callCommand )
import Text.Printf ( printf )

import Photoname.Common ( Ph, Options (..), ask, liftIO )


setArtist :: String -> Ph ()
setArtist newPath = do
  opts <- ask

  case optArtist opts of
    Nothing -> return ()
    Just "" ->
      setArtist' $ printf "exiv2 --Modify 'del Exif.Image.Artist' %s"
        newPath
    Just artistInfo ->
      setArtist' $ printf "exiv2 --Modify 'set Exif.Image.Artist %s' %s"
        artistInfo newPath


setArtist' :: String -> Ph ()
setArtist' commandLine = do
  opts <- ask

  -- Display what will be done
  unless (optQuiet opts) $ liftIO $ putStrLn $ commandLine

  -- Execute the command
  unless (optNoAction opts) $ liftIO $ callCommand commandLine

  return ()
