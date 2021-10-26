import Control.Monad ( filterM, forM_, when )
import System.Posix ( getFileStatus, isRegularFile )
import Text.Printf ( printf )

import Photoname.Common ( Ph, Options (..), runRename )
import Photoname.CopyLink ( createNewLink )
import Photoname.Date ( PhDate, parseExifDate, parseFilenameDate )
import Photoname.Exif ( getExifDate )
import Photoname.Exiv2 ( setArtist, setExifDate )
import Photoname.Opts ( parseOpts )


acquireDate :: FilePath -> Ph PhDate
acquireDate oldPath = do
  dateString <- getExifDate oldPath
  return $ mconcat
    [ parseExifDate dateString
    , parseFilenameDate oldPath
    ]


processFile :: FilePath -> Ph ()
processFile oldPath = do
  imageDate <- acquireDate oldPath
  newPath <- createNewLink imageDate oldPath
  setExifDate imageDate newPath
  setArtist newPath


-- Figure out and execute what the user wants based on the supplied args.
main :: IO ()
main = do
   opts <- parseOpts

   -- Get rid of anything not a regular file from the list of paths
   actualPaths <- filterM
      (\p -> getFileStatus p >>= return . isRegularFile) (optPaths opts)

   -- Notify user of the switches that will be in effect.
   when (optNoAction opts) $
      putStrLn "No-action mode, nothing will be changed."

   when (optMove opts) $
      putStrLn "Removing original links after new links are in place."

   -- Do the link manipulations, and report any errors.
   forM_ actualPaths $ \path -> do
      result <- runRename opts $ processFile path
      either (\em -> printf "** Processing %s: %s\n" path em)
         (const return ()) result

   -- Perhaps we should get an ExitCode back from all this above?
