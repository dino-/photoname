{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}

module Photoname.Date
  ( PhDate (..)
  , formatYear, formatDateHyphens, formatDate, formatDateTime
  , formatDateForExif
  , parseExifDate
  , parseFilenameDate
  )
  where

import Control.Newtype.Generics ( op )
import Data.Functor.Identity ( Identity )
import Data.Time.Calendar ( fromGregorian )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( LocalTime (..), TimeOfDay (..) )
import System.FilePath ( takeFileName )
import Text.Parsec ( ParsecT )
import Text.ParserCombinators.Parsec ( anyChar, char, count, digit,
  lookAhead, manyTill, parse, space, try )

import Photoname.Common ( SrcPath (..) )


data PhDate
  = ExifDate LocalTime
  | FilenameDate LocalTime
  | NoDateFound
  deriving (Eq, Show)

instance Semigroup PhDate where
  (<>) e@(ExifDate _)     _              = e
  (<>)   (FilenameDate _) e@(ExifDate _) = e
  (<>) f@(FilenameDate _) _              = f
  (<>)    NoDateFound     x              = x

instance Monoid PhDate where
  mempty = NoDateFound


-- Parsec helper defs

colon :: ParsecT String u Identity Char
colon = char ':'

digit2, digit4 :: ParsecT String u Identity [Char]
digit2 = count 2 digit
digit4 = count 4 digit


{- Parse a string in the form "yyyy:mm:dd hh:mm:ss" into a 
   CalendarTime datatype. Strings that fail to parse in this manner are
   returned as Nothing
-}
parseExifDate :: Maybe String -> PhDate
parseExifDate Nothing = NoDateFound
parseExifDate (Just s) =
   case (parse dateParser "" s) of
      Left _  -> NoDateFound
      Right x -> ExifDate x
   where
      dateParser = do
         year <- digit4 ; colon ; month <- digit2 ; colon ; day <- digit2
         space
         hour <- digit2 ; colon ; minute <- digit2
         colon ; second <- digit2
         pure $
            LocalTime
               (fromGregorian (read year) (read month) (read day))
               (TimeOfDay (read hour) (read minute)
                  (fromIntegral ((read second) :: Integer)))


{- Parse a string in one of the the forms below into a CalendarTime datatype.

    /some/path/ANYTHINGyyyy-mm-dd-hhmmss.jpg
    /some/path/ANYTHINGyyyy-mm-dd-hh-mm-ss-ttt.jpg
    ANYTHINGyyyy-mm-dd-hhmmss.jpg
    ANYTHINGyyyy-mm-dd-hh-mm-ss-ttt.jpg
    yyyymmdd-hhmmss.jpg
    yyyymmdd-hhmmss_xyz.jpg
    PXL_yyyymmdd_hhmmssttt.jpg
    PXL_yyyymmdd_hhmmssttt_xyz.jpg
-}
parseFilenameDate :: SrcPath -> PhDate
parseFilenameDate srcPath =
  case (parse dateParser "" (takeFileName . op SrcPath $ srcPath)) of
    Left _  -> NoDateFound
    Right x -> FilenameDate x
  where
    dateParser = do
      manyTill anyChar (try $ lookAhead digit4)
      year <- digit4
      manyTill anyChar (try $ lookAhead digit2)
      month <- digit2
      manyTill anyChar (try $ lookAhead digit2)
      day <- digit2
      manyTill anyChar (try $ lookAhead digit2)
      hour <- digit2
      manyTill anyChar (try $ lookAhead digit2)
      minute <- digit2
      manyTill anyChar (try $ lookAhead digit2)
      second <- digit2
      pure $
         LocalTime
            (fromGregorian (read year) (read month) (read day))
            (TimeOfDay (read hour) (read minute)
               (fromIntegral ((read second) :: Integer)))


{- Format a Maybe CalendarTime into a "yyyy" string
-}
formatYear :: LocalTime -> String
formatYear = formatTime defaultTimeLocale "%Y"


{- Format a Maybe CalendarTime into a "yyyy-mm-dd" string
-}
formatDateHyphens :: LocalTime -> String
formatDateHyphens = formatTime defaultTimeLocale "%Y-%m-%d"


{- Format a Maybe CalendarTime into a "yyyymmdd" string
-}
formatDate :: LocalTime -> String
formatDate = formatTime defaultTimeLocale "%Y%m%d"


{- Format a Maybe CalendarTime into a "yyyymmdd-HHMMSS" string
-}
formatDateTime :: LocalTime -> String
formatDateTime = formatTime defaultTimeLocale "%Y%m%d-%H%M%S"


{- Format a Maybe CalendarTime into a "yyyy:mm:dd HH:MM:SS" string
-}
formatDateForExif :: LocalTime -> String
formatDateForExif = formatTime defaultTimeLocale "%Y:%m:%d %H:%M:%S"
