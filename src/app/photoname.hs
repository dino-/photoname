{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad ( filterM, forM_, when )
import Data.Functor ( (<&>) )
import System.Posix ( FileStatus, getFileStatus, isRegularFile )
import Text.Printf ( printf )

import Photoname.Common ( CopySwitch ( v),
  Extension ( Extension, UseExistingExtension), Links, MoveSwitch ( v),
  NoActionSwitch ( v), Options ( copy, extension, links, move, noAction, paths,
  verbosity), Ph, SrcPath ( SrcPath, v), liftIO, runRename )
import Photoname.CopyLink ( createNewLink )
import Photoname.Date ( PhDate, parseExifDate, parseFilenameDate )
import Photoname.Exif ( getExifDate )
import Photoname.Exiv2 ( getExifDateWithExiv2, setArtist, setExifDate )
import Photoname.Links ( describeHardLinkPolicy, linksTest )
import Photoname.Log ( errorM, initLogging, infoM, lname )
import Photoname.Opts ( parseOpts )


acquireDate :: SrcPath -> Ph PhDate
acquireDate srcPath = do
  mconcat <$> sequence
    [ parseExifDate <$> getExifDate srcPath
    , parseExifDate <$> getExifDateWithExiv2 srcPath
    , pure . parseFilenameDate $ srcPath
    ]


processFile :: SrcPath -> Ph ()
processFile srcPath = do
  liftIO . infoM lname $ "----------"
  imageDate <- acquireDate srcPath
  destPath <- createNewLink imageDate srcPath
  setExifDate imageDate destPath
  setArtist destPath


-- Get rid of anything not a regular file or with a number of links that
-- doesn't match our links amount which is either passed in by the user with
-- the -l|--links switch or any number of links is fine.
filterWantedFiles :: Links -> [FilePath] -> IO [SrcPath]
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
      infoM lname "No-action mode, nothing will be changed"

   when opts.copy.v $
      infoM lname "Files will be copied instead of the default of hard linking"

   case opts.extension of
      Extension ext -> infoM lname $ printf "The extension '%s' will be used for all files" ext
      UseExistingExtension -> pure ()

   when opts.move.v $
      infoM lname "Removing original links after new links are in place"

   describeHardLinkPolicy $ links opts

   actualPaths <- filterWantedFiles (links opts) (paths opts)

   -- Do the link manipulations, and report any errors.
   forM_ actualPaths $ \srcPath -> do
      result <- runRename opts $ processFile srcPath
      either
        {- HLINT ignore "Avoid lambda" -}
        -- Because the compiler can't figure out printf is expecting an argument at compile time
        (\em -> errorM lname $ printf "** Processing %s: %s" srcPath.v em)
        pure result

   -- Perhaps we should get an ExitCode back from all this above?
