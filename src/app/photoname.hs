import Control.Monad ( filterM, forM_, when )
import Control.Newtype.Generics ( op )
import Data.Functor ( (<&>) )
import System.Posix ( FileStatus, getFileStatus, isRegularFile )
import Text.Printf ( printf )

import Photoname.Common ( CopySwitch (..), Links (Exactly, NoLimit),
  MoveSwitch (..), NoActionSwitch (..), Options (..), Ph, SrcPath (..),
  linksTest, runRename )
import Photoname.CopyLink ( createNewLink )
import Photoname.Date ( PhDate, parseExifDate, parseFilenameDate )
import Photoname.Exif ( getExifDate )
import Photoname.Exiv2 ( setArtist, setExifDate )
import Photoname.Log ( errorM, initLogging, infoM, lname )
import Photoname.Opts ( parseOpts )


acquireDate :: SrcPath -> Ph PhDate
acquireDate srcPath = do
  dateString <- getExifDate srcPath
  pure $ mconcat
    [ parseExifDate dateString
    , parseFilenameDate srcPath
    ]


processFile :: SrcPath -> Ph ()
processFile srcPath = do
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

   initLogging $ optVerbosity opts

   -- Notify user of the switches that will be in effect.
   when (op NoActionSwitch . optNoAction $ opts) $
      infoM lname "No-action mode, nothing will be changed."

   when (op CopySwitch . optCopy $ opts) $
      infoM lname "Copy has been specified instead of the default of hard linking."

   when (op MoveSwitch . optMove $ opts) $
      infoM lname "Removing original links after new links are in place."

   case optLinks opts of
      Exactly l -> infoM lname $ printf "Only processing files with %s hard links" (show l)
      NoLimit   -> pure ()

   actualPaths <- filterWantedFiles (optLinks opts) (optPaths opts)

   -- Do the link manipulations, and report any errors.
   forM_ actualPaths $ \srcPath -> do
      result <- runRename opts $ processFile srcPath
      either
        {- HLINT ignore "Avoid lambda" -}
        -- Because the compiler can't figure out printf is expecting an argument at compile time
        (\em -> errorM lname $ printf "** Processing %s: %s\n" (op SrcPath srcPath) em)
        pure result

   -- Perhaps we should get an ExitCode back from all this above?
