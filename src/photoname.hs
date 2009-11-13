-- Copyright: 2007-2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Control.Monad.Reader
import Graphics.Exif ( fromFile, getTag )
import System.Environment ( getArgs )
import System.FilePath
import System.Posix

import Photoname.Date
import Photoname.Opts ( Options (..) , parseOpts, usageText )
import Photoname.Serial ( getSerial )


type Ph a = ReaderT Options IO a


runRename :: Options -> Ph a -> IO a
runRename env action = runReaderT action env


modeDir :: FileMode
modeDir = ownerModes       `unionFileModes`
          groupReadMode    `unionFileModes`
          groupExecuteMode


{- Get shoot date from the exif information. There are several tags 
   potentially containing dates. Try them in a specific order until we
   find one that has data.
-}
getDate :: (MonadError String m, MonadIO m) => FilePath -> m String
getDate path = do
   exif <- liftIO $ fromFile path

   -- This foldl gets us the first IO (Maybe String) that's not Nothing
   maybeDate <- liftIO $ foldl (liftM2 mplus) (return Nothing) $
      map (getTag exif)
      ["DateTimeDigitized", "DateTimeOriginal", "DateTime"]

   case maybeDate of
      Just d -> return d
      Nothing  -> throwError $ "File " ++ path ++ " has no EXIF date"

{- Take a file path to a JPEG file and use EXIF information available to
   move the file to a new location below the given basedir.
-}
createNewLink :: FilePath -> FilePath -> Ph ()
createNewLink newDir oldPath = do
   opts <- ask
   result <- liftIO $ runErrorT $ do
      newPath <- buildNewPath newDir oldPath

      -- Check for existance of the target file
      exists <- liftIO $ fileExist newPath
      when exists $ throwError $
         "** " ++ oldPath ++ " -> " ++ newPath ++ " exists!"

      -- Display what will be done
      unless (optQuiet opts) $ 
         liftIO $ putStrLn $ oldPath ++ " -> " ++ newPath

      unless (optNoAction opts) $ do
         -- Make the target dir
         liftIO $ makeDirectory $ takeDirectory newPath

         -- Make the new hard link
         liftIO $ createLink oldPath newPath

         -- If user has specified, remove the original link
         when (optMove opts) $
            liftIO $ removeLink oldPath

      return ()

   case result of
      Left errMsg -> liftIO $ putStrLn errMsg
      Right _ -> return ()


{- Given a list of lists, make a new list where each sublist element 
   consists of the accumulation of all parts that came before it. 
   Like this:
      before: [[1], [2], [3], [4], [5]]
      after : [[1], [1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5]]

   Many thanks to Betty Diegel for help with this algorithm.

   FIXME Try to rewrite this with a scan or something
-}
listAcc :: [[a]] -> [[a]]
listAcc []     = [[]]
listAcc (x:xs) = listAcc' x xs
   where
      listAcc' l (y:ys) = [l] ++ listAcc' (l ++ y) ys
      listAcc' l []     = [l]


{- Ensuring that a directory with subs exists turned out to be a painful 
   process involving making each parent dir piece by piece but not trying
   to make anything that's already there.
-}
makeDirectory :: FilePath -> IO ()
makeDirectory d =
   let makeOneDir dir = do
         exists <- fileExist dir
         unless exists $ createDirectory dir modeDir
   in  mapM_ makeOneDir $ listAcc $ splitPath d


{- Given a path to a file with EXIF data, construct a new path based on the
   date and some serial number info we can parse out of the filename.
-}
buildNewPath :: (MonadError String m, MonadIO m) =>
   FilePath -> FilePath -> m FilePath
buildNewPath newDir oldPath = do
   dateString <- getDate oldPath
   serial <- getSerial oldPath
   let date = readDate dateString
   let year = formatYear date
   let day = formatDay date
   let prefix = formatPrefix date
   return (newDir </> year </> day </>
      (prefix ++ "_" ++ serial) <.> "jpg")


-- Figure out and execute what the user wants based on the supplied args.
executeCommands :: [String] -> Ph ()

-- User gave no files at all. Display help
executeCommands [] = liftIO $ putStrLn usageText

-- Normal program operation, process the files with the args.
executeCommands (dir:filePaths) = do
   opts <- ask

   -- Get rid of anything not a regular file from the list of paths
   actualPaths <- liftIO $ filterM
      (\p -> getFileStatus p >>= return . isRegularFile) filePaths

   -- Notify user of the switches that will be in effect.
   when (optNoAction opts) $
      liftIO $ putStrLn "No-action mode, nothing will be changed."

   when (optMove opts) $
      liftIO $ putStrLn 
         "Removing original links after new links are in place."

   -- Do the link manipulations.
   mapM_ (createNewLink dir) actualPaths


main :: IO ()
main = do
   -- Parse the arguments
   (opts, paths) <- getArgs >>= parseOpts

   -- Do the photo naming procedure
   runRename opts $ executeCommands paths

   -- Perhaps we should get an ExitCode back from all this above?
