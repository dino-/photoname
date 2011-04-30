-- Copyright: 2007-2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Photoname.SerialFormat
   ( buildSerialPath
   )
   where

import System.FilePath
import Text.ParserCombinators.Parsec

import Photoname.Common
import Photoname.Date
import Photoname.Exif
import Photoname.Opts ( Options (..) )


{- Combinator similar to manyTill, but evaluates to end instead of p
-}
skipTill :: GenParser tok st t1 -> GenParser tok st t -> GenParser tok st t
skipTill p end = scan
   where
      scan = do { end }     -- If end succeeds, return that.
             <|>
             do { p; scan } -- Otherwise, match p and recurse.


serialNum :: GenParser Char st [Char]
serialNum =
   skipTill anyChar $ try $ do  -- Skip anything up to..
      serial <- count 3 digit   -- 3 digits..
      char '.'                  -- followed by a period..
      count 3 alphaNum          -- followed by 3 alphaNumS..
      eof                       -- at the end of the string.
      return serial             -- Return those 3 digits


{- Evaluates one or more parsers trying to find the serial number in the
   supplied path. Transforms the result from Either to Maybe
-}
getSerial :: (MonadError String m) => String -> m String
getSerial path =
   case (parse serialNum "" path) of
      Left _  -> throwError "Can't determine serial"
      Right serial -> return serial


{- Given a path to a file with EXIF data, construct a new path based on the
   date and some serial number info we can parse out of the filename.
-}
buildSerialPath :: FilePath -> Ph FilePath
buildSerialPath oldPath = do
   dateString <- getDate oldPath
   serial <- getSerial oldPath
   let date = readDate dateString

   suffix <- asks optSuffix
   let fileName = (formatDate date) ++ "_" ++ serial
         ++ suffix <.> "jpg"

   parentDir <- asks optParentDir
   noDirs <- asks optNoDirs
   return $ if (noDirs)
      then parentDir </> fileName
      else parentDir </> (formatYear date) </>
         (formatDateHyphens date) </> fileName
