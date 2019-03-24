{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Photoname.Date
   ( formatYear, formatDateHyphens, formatDate, formatDateTime
   , readDate
   )
   where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec


{- Parse a date string in the form "yyyy:mm:dd hh:mm:ss" into a 
   CalendarTime datatype. Strings that fail to parse in this manner are
   returned as Nothing
-}
readDate :: String -> Maybe LocalTime
readDate s =
   case (parse dateParser "" s) of
      Left _  -> Nothing
      Right x -> Just x
   where
      digit4 = count 4 digit
      digit2 = count 2 digit
      colon = char ':'
      dateParser = do
         year <- digit4 ; colon ; month <- digit2 ; colon ; day <- digit2
         space
         hour <- digit2 ; colon ; minute <- digit2
         colon ; second <- digit2
         return $
            LocalTime
               (fromGregorian (read year) (read month) (read day))
               (TimeOfDay (read hour) (read minute)
                  (fromIntegral ((read second) :: Integer)))


{- Format a Maybe CalendarTime into a "yyyy" string. Dates that are
   Nothing in value format to "0000"
-}
formatYear :: Maybe LocalTime -> String
formatYear Nothing  = "0000"
formatYear (Just x) = formatTime defaultTimeLocale "%Y" x


{- Format a Maybe CalendarTime into a "yyyy-mm-dd" string. Dates that are
   Nothing in value format to "0000-00-00"
-}
formatDateHyphens :: Maybe LocalTime -> String
formatDateHyphens Nothing  = "0000-00-00"
formatDateHyphens (Just x) = formatTime defaultTimeLocale "%Y-%m-%d" x


{- Format a Maybe CalendarTime into a "yyyymmdd" string. Dates that are
   Nothing in value format to "00000000"
-}
formatDate :: Maybe LocalTime -> String
formatDate Nothing  = "00000000"
formatDate (Just x) = formatTime defaultTimeLocale "%Y%m%d" x


{- Format a Maybe CalendarTime into a "yyyymmdd-HHMMSS" string. Dates 
   that are Nothing in value format to "00000000-000000"
-}
formatDateTime :: Maybe LocalTime -> String
formatDateTime Nothing  = "00000000-000000"
formatDateTime (Just x) = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" x
