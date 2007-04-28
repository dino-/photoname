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
   | Move
   | Help
   deriving Eq


options :: [OptDescr Flag]
options =
   [
     Option ['n'] ["no-action"] (NoArg NoAction) 
         "Display what would be done, but do nothing"
   , Option ['q'] ["quiet"] (NoArg Quiet) 
         "Suppress normal output of what's being done"
   , Option []    ["move"] (NoArg Move)
         "Move the files, don't just hard-link to the new locations"
   , Option ['h'] ["help"] (NoArg Help)
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
         [ "Usage: photoname [OPTIONS] PARENTDIR FILES"
         , "Rename and move photo files based on EXIF data"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "This software is for renaming and storing your digital photos. It will extract"
         , "shoot date from the EXIF data in the image file and use that along with the"
         , "usually-camera-assigned serial number to build a meaningful filename."
         , ""
         , "You get a subdirectory hierarchy consisting of directories for the years,"
         , "then subdirs within those for the day photos were shot. These day dirs"
         , "contain the image files, renamed as follows:"
         , ""
         , "A photo shot on 2002-May-02 with a Canon camera:"
         , "   img_1790.jpg -> <PARENTDIR>/2002/2002-05-02/20020502_790.jpg"
         , ""
         , "The <PARENTDIR> is the one given on the command-line to this utility, and"
         , "represents the top-level of where you're storing photos."
         , ""
         , "Note that the default behavior of this software is to create hard links to"
         , "the new paths and leave the original links as they were. You can use the"
         , "--move switch to blow away the original location, leaving only the new."
         , ""
         , "Version 002  2007-Apr-28  Dino Morelli <dino@ui3.info>"
         ]
