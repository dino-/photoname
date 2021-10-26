import Control.Monad ( filterM, forM_, unless, when )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( takeDirectory )
import System.Posix ( createLink, fileExist, getFileStatus,
   isRegularFile, removeLink )
import Text.Printf ( printf )

import Photoname.Common ( Ph, Options (..), ask, liftIO, runRename, throwError )
import Photoname.Date
  ( PhDate (ExifDate, FilenameDate, NoDateFound)
  , parseExifDate, parseFilenameDate
  )
import Photoname.Exif ( getExifDate )
import Photoname.Exiv2 ( setArtist, setExifDate )
import Photoname.Opts ( parseOpts )
import Photoname.DateFormat ( buildDatePath )


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


createNewLink :: PhDate -> FilePath -> Ph FilePath
createNewLink imageDate oldPath = do
  opts <- ask
  newPath <- case imageDate of
    ExifDate lt -> buildDatePath lt
    FilenameDate lt -> buildDatePath lt
    NoDateFound -> throwError "Could not extract any date information"

  -- Check for existance of the target file
  exists <- liftIO $ fileExist newPath
  when exists $ throwError $ "Destination " ++ newPath ++ " exists!"

  -- Display what will be done
  unless (optQuiet opts) $
    liftIO $ putStrLn $ oldPath ++ " -> " ++ newPath

  unless (optNoAction opts) $ do
    -- Make the target dir
    liftIO $ createDirectoryIfMissing True $ takeDirectory newPath

    -- Make the new hard link
    liftIO $ createLink oldPath newPath

    -- If user has specified, remove the original link
    when (optMove opts) $
       liftIO $ removeLink oldPath

  return newPath


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
