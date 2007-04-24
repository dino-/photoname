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

module Photoname.Opts (
   Flag (..),
   parseOpts, usageText
)
   where

import System.Console.GetOpt


data Flag
   = NoAction
   | Quiet
   | Help
   deriving Eq


options :: [OptDescr Flag]
options =
   [
     Option ['n'] ["no-action"]  (NoArg NoAction) 
         "Display what would be done, but do nothing"
   , Option ['q'] ["quiet"]  (NoArg Quiet) 
         "Suppress normal output of what's being done"
   , Option ['h'] ["help"]    (NoArg Help)
         "This help text"
   ]


parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageText))


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: photoname [OPTIONS] [FILES]"
         , "Rename and move photo files based on EXIF data"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ ""
         , "Version 001  2007-Apr-23  Dino Morelli <dino@ui3.info>"
         ]
