-- Copyright: 2007-2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module Photoname.Serial
   ( getSerial
   )
   where

import Control.Monad.Error
import Text.ParserCombinators.Parsec


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
      Left _  -> throwError $ "File " ++ path ++ " has no serial"
      Right serial -> return serial
