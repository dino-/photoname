-- Copyright: 2007, 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Photoname.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import System.Console.GetOpt


data Options = Options
   { optNoAction :: Bool
   , optQuiet :: Bool
   , optMove :: Bool
   , optHelp :: Bool
   }


defaultOptions :: Options
defaultOptions = Options
   { optNoAction = False
   , optQuiet = False
   , optMove = False
   , optHelp = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['n'] ["no-action"]
      (NoArg (\opts -> opts { optNoAction = True } )) 
      "Display what would be done, but do nothing"
   , Option ['q'] ["quiet"] 
      (NoArg (\opts -> opts { optQuiet = True } )) 
      "Suppress normal output of what's being done"
   , Option []    ["move"] 
      (NoArg (\opts -> opts { optMove = True } ))
      "Move the files, don't just hard-link to the new locations"
   , Option ['h'] ["help"] 
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   ]


parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageText)


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
         [ "This software is for renaming and storing your digital photos. It will extract shoot date from the EXIF data in the image file and use that along with the usually-camera-assigned serial number to build a meaningful filename."
         , ""
         , "You get a subdirectory hierarchy consisting of directories for the years, then subdirs within those for the day photos were shot. These day dirs contain the image files, renamed as follows:"
         , ""
         , "A photo shot on 2002-May-02 with a Canon camera:"
         , "   img_1790.jpg -> <PARENTDIR>/2002/2002-05-02/20020502_790.jpg"
         , ""
         , "The code is basically looking for three digits before the file extension to use as a 'serial' number for the day's photos. This is a seemingly common occurrance with cameras that we can pick numbers off the end of the filename, or at least I hope that's the case. Examples of the two that I have:"
         , ""
         , "                    vvv (use these digits)"
         , "   Panasonic:  P###0###.jpg"
         , "   Canon:      img_####.jpg"
         , "                    ^^^"
         , "The <PARENTDIR> is the one given on the command-line to this utility, and represents the top-level of where you're storing photos."
         , ""
         , "Note that the default behavior of this software is to create hard links to the new paths and leave the original links as they were. You can use the --move switch to blow away the original location, leaving only the new."
         , ""
         , "Version 2.1  2008-Oct-10  Dino Morelli <dino@ui3.info>"
         ]
