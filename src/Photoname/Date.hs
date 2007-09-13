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
   along with photoname; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  
   02110-1301  USA
-}

module Photoname.Date (
   formatYear, formatDay, formatPrefix,
   readDate
)
   where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale ( defaultTimeLocale )
import Text.ParserCombinators.Parsec


-- Parse a date string in the form "yyyy:mm:dd hh:mm:ss" into a 
-- CalendarTime datatype. Strings that fail to parse in this manner are
-- returned as Nothing
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


-- Format a Maybe CalendarTime into a "yyyy" string. Dates that are
-- Nothing in value format to "0000"
formatYear :: Maybe LocalTime -> String
formatYear Nothing  = "0000"
formatYear (Just x) = formatTime defaultTimeLocale "%Y" x


-- Format a Maybe CalendarTime into a "yyyymmdd" string. Dates that are
-- Nothing in value format to "00000000"
formatDay :: Maybe LocalTime -> String
formatDay Nothing  = "0000-00-00"
formatDay (Just x) = formatTime defaultTimeLocale "%Y-%m-%d" x


-- Format a Maybe CalendarTime into a "yyyymmdd" string. Dates that are
-- Nothing in value format to "00000000"
formatPrefix :: Maybe LocalTime -> String
formatPrefix Nothing  = "00000000"
formatPrefix (Just x) = formatTime defaultTimeLocale "%Y%m%d" x
