module Photoname.Serial (
   getSerial
)
   where

import Text.ParserCombinators.Parsec


-- Parses the syntax of Canon camera file paths for the serial number.
parserCanon :: GenParser Char st [Char]
parserCanon = do
   manyTill anyChar $ try (string "img_")
   digit
   count 3 digit


-- Evaluates one or more parsers trying to find the serial number in the
-- supplied path. Transforms the result from Either to Maybe
-- XXX Maybe a debug flag here for the error messages.
getSerial :: FilePath -> Maybe String
getSerial s =
   case (parse parser "" s) of
      Left _  -> Nothing
      Right x -> Just x
   where
      parser = parserCanon
