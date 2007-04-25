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

module Photoname.Serial (
   getSerial
)
   where

--import Debug.Trace
import Text.ParserCombinators.Parsec


-- Parses the syntax of Canon camera file paths for the serial number.
parserCanon :: GenParser Char st [Char]
parserCanon = do
   manyTill anyChar $ try (string "img_")
   digit
   count 3 digit


-- Evaluates one or more parsers trying to find the serial number in the
-- supplied path. Transforms the result from Either to Maybe
getSerial :: FilePath -> Maybe String
getSerial s =
   case (parse parser "" s) of
      Left _  -> Nothing
      --Left err  -> trace (show err) Nothing
      Right x -> Just x
   where
      parser = parserCanon
