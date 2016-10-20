-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad ( filterM, forM_, unless, when )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.FilePath ( takeDirectory )
import System.Posix ( FileMode, createLink, fileExist, getFileStatus,
   groupExecuteMode, groupReadMode, isRegularFile, ownerModes,
   removeLink, unionFileModes )
import Text.Printf ( printf )

import Photoname.Common ( Ph, ask, liftIO, runRename, throwError )
import Photoname.Opts ( Options (..) , parseOpts, usageText )
import Photoname.DateFormat ( buildDatePath )
import Photoname.SerialFormat ( buildSerialPath )


modeDir :: FileMode
modeDir = ownerModes       `unionFileModes`
          groupReadMode    `unionFileModes`
          groupExecuteMode


{- Take a file path to a JPEG file and use EXIF information available to
   link the file at its new location below the given basedir.
-}
createNewLink :: FilePath -> Ph ()
createNewLink oldPath = do
   opts <- ask
   newPath <- if (optOldStyle opts)
      then buildSerialPath oldPath
      else buildDatePath oldPath

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

      return ()


-- Figure out and execute what the user wants based on the supplied args.
executeCommands :: Options -> [String] -> IO ()

-- User gave no files at all. Display help
executeCommands _ [] = putStrLn usageText

-- Normal program operation, process the files with the args.
executeCommands opts filePaths = do
   -- Get rid of anything not a regular file from the list of paths
   actualPaths <- filterM
      (\p -> getFileStatus p >>= return . isRegularFile) filePaths

   -- Notify user of the switches that will be in effect.
   when (optNoAction opts) $
      putStrLn "No-action mode, nothing will be changed."

   when (optMove opts) $
      putStrLn "Removing original links after new links are in place."

   -- Do the link manipulations, and report any errors.
   forM_ actualPaths $ \path -> do
      result <- runRename opts $ createNewLink path
      either (\em -> printf "** Processing %s: %s\n" path em)
         (const return ()) result


main :: IO ()
main = do
   -- Parse the arguments
   (opts, paths) <- getArgs >>= parseOpts

   -- Do the photo naming procedure
   executeCommands opts paths

   -- Perhaps we should get an ExitCode back from all this above?
