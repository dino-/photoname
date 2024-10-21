{-# LANGUAGE OverloadedRecordDot, OverloadedStrings #-}

import Control.Monad (filterM, forM_, when)
import Formatting ((%), (%+), formatToString, string)
import Data.Functor ((<&>))
import System.Posix (FileStatus, getFileStatus, isRegularFile)

import Photoname.Common (CopySwitch (v),
  Extension (Extension, UseExistingExtension), Links, MoveSwitch (v),
  NoActionSwitch (v), Options (copy, extension, links, move, noAction, paths,
  verbosity), Ph, SrcPath (SrcPath, v), liftIO, runRename)
import Photoname.CopyLink (createNewLink)
import Photoname.Date (PhDate, parseExifDate, parseFilenameDate)
import Photoname.Exif (getExifDate)
import Photoname.Exiv2 (getExifDateWithExiv2, setArtist, setExifDate)
import Photoname.Links (describeHardLinkPolicy, linksTest)
import Photoname.Log (errorM, initLogging, lname, noticeM)
import Photoname.Opts (parseOpts)


acquireDate :: SrcPath -> Ph PhDate
acquireDate srcPath = do
  mconcat <$> sequence
    [ parseExifDate <$> getExifDate srcPath
    , parseExifDate <$> getExifDateWithExiv2 srcPath
    , pure . parseFilenameDate $ srcPath
    ]


processFile :: SrcPath -> Ph ()
processFile srcPath = do
  liftIO . noticeM lname $ "----------"
  imageDate <- acquireDate srcPath
  destPath <- createNewLink imageDate srcPath
  setExifDate imageDate destPath
  setArtist destPath


-- Get rid of anything not a regular file or with a number of links that
-- doesn't match our links amount which is either passed in by the user with
-- the -l|--links switch or any number of links is fine.
filterWantedFiles :: Links -> [FilePath] -> IO [SrcPath]
{- HLINT ignore "Use fmap" -}
filterWantedFiles links inputFiles = map SrcPath <$> filterM (\p ->
    getFileStatus p <&> testFile) inputFiles
  where
    testFile :: FileStatus -> Bool
    testFile fileStatus = isRegularFile fileStatus && linksTest links fileStatus


-- Figure out and execute what the user wants based on the supplied args.
main :: IO ()
main = do
   opts <- parseOpts

   initLogging $ verbosity opts

   -- Notify user of the switches that will be in effect.
   when opts.noAction.v $
      noticeM lname "No-action mode, nothing will be changed"

   when opts.copy.v $
      noticeM lname "Files will be copied instead of the default of hard linking"

   case opts.extension of
      Extension ext -> noticeM lname $ formatToString ("The extension '" % string % "' will be used for all files") ext
      UseExistingExtension -> pure ()

   when opts.move.v $
      noticeM lname "Removing original links after new links are in place"

   describeHardLinkPolicy $ links opts

   actualPaths <- filterWantedFiles (links opts) (paths opts)

   -- Do the link manipulations, and report any errors.
   forM_ actualPaths $ \srcPath -> do
      result <- runRename opts $ processFile srcPath
      either
        (errorM lname . formatToString ("** Processing" %+ string % ":" %+ string) srcPath.v)
        pure result

   -- Perhaps we should get an ExitCode back from all this above?
