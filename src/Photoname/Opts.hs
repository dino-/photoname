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
         [ "I have been through several picture naming and directory layout debacles over"
         , "the years. My most recent decision was to try the scheme detailed by one"
         , "Calvin Hass on his Impulse Adventure site"
         , "(http://www.impulseadventure.com/photo/flow-name-file.html)"
         , ""
         , "The basic idea is this: If I have a file named (by my Canon camera)"
         , "img_1582.jpg and which was shot on 2007-04-23, this file would be linked"
         , "to or moved to this path: <top-level>/2007/2007-04-23/20070423_582.jpg"
         , ""
         , "Version 001  2007-Apr-23  Dino Morelli <dino@ui3.info>"
         ]
