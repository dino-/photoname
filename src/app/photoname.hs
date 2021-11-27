import Control.Monad ( filterM, forM_, when )
import Control.Newtype.Generics ( op )
import Data.Functor ( (<&>) )
import System.Posix ( getFileStatus, isRegularFile )
import Text.Printf ( printf )

import Photoname.Common ( CopySwitch (..), MoveSwitch (..),
  NoActionSwitch (..), Options (..), Ph, SrcPath (..), runRename )
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


-- Figure out and execute what the user wants based on the supplied args.
main :: IO ()
main = do
   opts <- parseOpts

   initLogging $ optVerbosity opts

   -- Get rid of anything not a regular file from the list of paths
   actualPaths <- map SrcPath <$> filterM
      (\p -> getFileStatus p <&> isRegularFile) (optPaths opts)

   -- Notify user of the switches that will be in effect.
   when (op NoActionSwitch . optNoAction $ opts) $
      infoM lname "No-action mode, nothing will be changed."

   when (op CopySwitch . optCopy $ opts) $
      infoM lname "Copy has been specified instead of the default of hard linking."

   when (op MoveSwitch . optMove $ opts) $
      infoM lname "Removing original links after new links are in place."

   -- Do the link manipulations, and report any errors.
   forM_ actualPaths $ \srcPath -> do
      result <- runRename opts $ processFile srcPath
      either
        {- HLINT ignore "Avoid lambda" -}
        -- Because the compiler can't figure out printf is expecting an argument at compile time
        (\em -> errorM lname $ printf "** Processing %s: %s\n" (op SrcPath srcPath) em)
        pure result

   -- Perhaps we should get an ExitCode back from all this above?
