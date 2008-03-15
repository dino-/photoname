-- Copyright: 2007, 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Main
   where

import Control.Monad.Error ()
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Graphics.Exif as Exif
import Photoname.Date
import qualified Photoname.Opts as Opts
import Photoname.Serial ( getSerial )
import System.Environment ( getArgs )
import System.FilePath
import System.Posix


modeDir :: FileMode
modeDir = ownerModes       `unionFileModes`
          groupReadMode    `unionFileModes`
          groupExecuteMode


{- Given a list of lists, make a new list where each sublist element 
   consists of the accumulation of all parts that came before it. 
   Like this:
      before: [[1], [2], [3], [4], [5]]
      after : [[1], [1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5]]

   Many thanks to Betty Diegel for help with this algorithm.
-}
listAcc :: [[a]] -> [[a]]
listAcc []     = [[]]
listAcc (x:xs) = listAcc' x xs
   where
      listAcc' l (y:ys) = [l] ++ listAcc' (l ++ y) ys
      listAcc' l []     = [l]


{- Execute a sequence of m (Maybe a) actions until the first non-Nothing
   evaluation, eval to that.

   Thanks to sjanssen et al on #haskell

   XXX Can this be genericized to be :: (Monad m, Monad n) =>
       [m (n a)] -> m (n a)
   XXX Put this in a more common module.
-}
firstSuccess :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstSuccess = foldr f (return Nothing)
   where
      f :: Monad t => t (Maybe m1) -> t (Maybe m1) -> t (Maybe m1)
      f m ms = do  -- m in here, NOT Maybe
         x <- m
         case x of
            Just _  -> return x
            Nothing -> ms


{- Get shoot date from the exif information. There are several tags 
   potentially containing dates. Try them in a specific order until we
   find one that has data.
-}
getDate :: FilePath -> IO (Either String String)
getDate path = do
   exif <- Exif.fromFile path
   maybeDate <- firstSuccess $ map (Exif.getTag exif)
      ["DateTimeDigitized", "DateTimeOriginal", "DateTime"]
   case maybeDate of
      Just d -> return $ Right d
      Nothing  -> return $ Left $ "File " ++ path ++ " has no EXIF date"


{- Take a file path to a JPEG file and use EXIF information available to 
   move the file to a new location below the given basedir.
-}
createNewLink :: FilePath -> FilePath -> Ph ()
createNewLink newDir oldPath = do
   flags <- ask
   e <- liftIO $ buildNewPath newDir oldPath
   case e of
      Left err      -> tell [err]
      Right newPath -> do
         -- Check for existance of the target file
         exists <- liftIO $ fileExist newPath
         if exists
            then tell
               ["** " ++ oldPath ++ " -> " ++ newPath ++ " exists!"]
            else do
               -- Display what will be done
               unless (Opts.Quiet `elem` flags) $ 
                  tell [(oldPath ++ " -> " ++ newPath)]

               unless (Opts.NoAction `elem` flags) $ do
                  -- Make the target dir
                  liftIO $ makeDirectory $ takeDirectory newPath

                  -- Make the new hard link
                  liftIO $ createLink oldPath newPath

                  -- If user has specified, remove the original link
                  when (Opts.Move `elem` flags) $
                     liftIO $ removeLink oldPath


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
buildNewPath :: FilePath -> FilePath -> IO (Either String FilePath)
buildNewPath newDir oldPath = do  -- IO
   eitherDateString <- getDate oldPath
   return $ do  -- Either String
      dateString <- eitherDateString
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
executeCommands [] = tell [Opts.usageText]

-- Normal program operation, process the files with the args.
executeCommands (dir:filePaths) = do
   flags <- ask

   -- Get rid of anything not a regular file from the list of paths
   actualPaths <- liftIO $ filterM
      (\p -> getFileStatus p >>= return . isRegularFile) filePaths

   -- Notify user of the switches that will be in effect.
   when (Opts.NoAction `elem` flags) $
      tell ["No-action mode, nothing will be changed."]

   when (Opts.Move `elem` flags) $
      tell ["Removing original links after new links are in place."]

   -- Do the link manipulations.
   mapM_ (createNewLink dir) actualPaths


type Ph a = ReaderT [Opts.Flag] (WriterT [String] IO) a

runRename :: [Opts.Flag] -> Ph a -> IO (a, [String])
runRename env exec = runWriterT $ runReaderT exec env

main :: IO ()
main = do
   -- Parse the arguments
   (flags, paths) <- getArgs >>= Opts.parseOpts

   -- Do the photo naming procedure
   (rval, messages) <- runRename flags $ executeCommands paths

   -- Display collected output log
   putStrLn $ unlines messages

   return rval
