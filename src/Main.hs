module Main
   where

import Control.Monad ( unless )
import Data.Maybe ( fromMaybe )
import qualified Graphics.Exif as Exif
import Photoname.Date
import Photoname.Serial
import System.Environment ( getArgs )
import System.FilePath
import System.Posix
import Text.Printf ( printf )


modeDir :: FileMode
modeDir = ownerModes       `unionFileModes`
          groupReadMode    `unionFileModes`
          groupExecuteMode


modeFile :: FileMode
modeFile = ownerReadMode  `unionFileModes`
           ownerWriteMode `unionFileModes`
           groupReadMode


{- Given a list of lists, make a new list where each sublist element 
   consists of the accumulation of all parts that came before it. 
   Like this:
      before: [[1], [2], [3], [4], [5]]
      after : [[1], [1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5]]
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
getDate :: Exif.Exif -> IO (Maybe String)
getDate exif =
   firstSuccess $ map (Exif.getTag exif)
      ["DateTimeDigitized", "DateTimeOriginal", "DateTime"]


{- Take a file path to a JPEG file and use EXIF information available to 
   move the file to a new location below the given basedir.
-}
createNewLink :: FilePath -> FilePath -> IO ()
createNewLink newDir oldPath = do
   e <- buildNewPath newDir oldPath
   case e of
      -- XXX Print this to STDERR
      Left err      -> putStrLn err
      Right newPath -> do
         -- Check for existance of the target file
         exists <- fileExist newPath
         if exists
            then putStrLn $ newPath ++ " exists!"
            else do
               -- Display what will be done
               putStrLn (oldPath ++ " -> " ++ newPath)

               -- Make the target dir
               makeDirectory $ takeDirectory newPath

               -- Make the new hard link
               createLink oldPath newPath

               -- Set permissions on the new link
               -- Don't need this? Perms come from original file?
               -- Need this later when we move files.
               --setFileMode newPath modeFile


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


buildNewPath :: FilePath -> FilePath -> IO (Either String FilePath)
buildNewPath newDir oldPath = do
   exif <- Exif.fromFile oldPath
   maybeDs <- getDate exif
   case maybeDs of
      Just ds  -> do
         let date = readDate ds
         let maybeSerial = getSerial oldPath
         case maybeSerial of
            Just serial -> do
               let year = formatYear date
               let day = formatDay date
               let prefix = formatPrefix date
               return $ Right $ newDir </> year </> day </> 
                  (prefix ++ "_" ++ serial) <.> "jpg"
            Nothing -> return $ Left $ "File " ++ oldPath ++ " has no serial."
      Nothing  -> return $ Left $ "File " ++ oldPath ++ " has no EXIF date."


main :: IO ()
main = do
   (dir:filePaths) <- getArgs
   mapM_ (createNewLink dir) filePaths
