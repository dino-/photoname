{-
   Copyright 2007 Dino Morelli

   This file is part of photoname

   photoname is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   photoname is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Foobar; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  
   02110-1301  USA
-}

module Main
   where

import Control.Monad ( filterM, unless, when )
import qualified Graphics.Exif as Exif
import Photoname.Date
import qualified Photoname.Opts as Opts
import Photoname.Serial
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
getDate :: Exif.Exif -> IO (Maybe String)
getDate exif =
   firstSuccess $ map (Exif.getTag exif)
      ["DateTimeDigitized", "DateTimeOriginal", "DateTime"]


{- Take a file path to a JPEG file and use EXIF information available to 
   move the file to a new location below the given basedir.
-}
createNewLink :: [Opts.Flag] -> FilePath -> FilePath -> IO ()
createNewLink flags newDir oldPath = do
   e <- buildNewPath newDir oldPath
   case e of
      Left err      -> putStrLn err
      Right newPath -> do
         -- Check for existance of the target file
         exists <- fileExist newPath
         if exists
            then putStrLn $
               "** " ++ oldPath ++ " -> " ++ newPath ++ " exists!"
            else do
               -- Display what will be done
               unless (Opts.Quiet `elem` flags) $ 
                  putStrLn (oldPath ++ " -> " ++ newPath)

               unless (Opts.NoAction `elem` flags) $ do
                  -- Make the target dir
                  makeDirectory $ takeDirectory newPath

                  -- Make the new hard link
                  createLink oldPath newPath

                  -- If user has specified, remove the original link
                  when (Opts.Move `elem` flags) $
                     removeLink oldPath


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


-- Figure out and execute what the user wants based on the supplied args.
executeCommands :: ([Opts.Flag], [String]) -> IO ()

-- User requested help. Display it and that's it
executeCommands ((Opts.Help:_), _) = putStrLn Opts.usageText

-- User gave no files at all. Display help
executeCommands (_, []) = putStrLn Opts.usageText

-- Normal program operation, process the files with the args.
executeCommands (flags, (dir:filePaths)) = do
   -- Get rid of anything not a regular file from the list of paths
   actualPaths <- filterM
      (\p -> getFileStatus p >>= return . isRegularFile) filePaths

   when (Opts.NoAction `elem` flags) $
      putStrLn "No-action mode, nothing will be changed."

   mapM_ (createNewLink flags dir) actualPaths


main :: IO ()
main = do
   args <- getArgs
   Opts.parseOpts args >>= executeCommands
