module Photoname.Opts
   ( Options (..)
   , parseOpts, usageText
   , formattedVersion
   )
   where

import Data.Version ( showVersion )
import Paths_photoname ( version )
import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg)
  , ArgOrder (Permute)
  , OptDescr (Option)
  , getOpt, usageInfo
  )
import System.Directory ( doesFileExist )


data Options = Options
   { optConfig :: String
   , optHelp :: Bool
   , optMove :: Bool
   , optNoAction :: Bool
   , optNoConfig :: Bool
   , optNoDirs :: Bool
   , optParentDir :: String
   , optQuiet :: Bool
   , optSuffix :: String
   , optVersion :: Bool
   }

defaultConfig :: String
defaultConfig = "~/.config/photoname.conf"

defaultOptions :: Options
defaultOptions = Options
   { optConfig = defaultConfig
   , optHelp = False
   , optMove = False
   , optNoAction = False
   , optNoConfig = False
   , optNoDirs = False
   , optParentDir = "."
   , optQuiet = False
   , optSuffix = ""
   , optVersion = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['c'] ["config"]
      (ReqArg (\c opts -> opts { optConfig = c }) "FILE")
      ("Defaults to " ++ defaultConfig ++ ". See CONFIG")
   , Option ['C'] ["no-config"]
      (NoArg (\opts -> opts { optNoConfig = True } ))
      "Do not load config file"
   , Option ['D'] ["no-dirs"]
      (NoArg (\opts -> opts { optNoDirs = True } ))
      "No subdirectory hierarchy. Just do DIR/NEWFILE"
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option []    ["move"]
      (NoArg (\opts -> opts { optMove = True } ))
      "Move the files, don't just hard-link to the new locations"
   , Option ['n'] ["no-action"]
      (NoArg (\opts -> opts { optNoAction = True } ))
      "Display what would be done, but do nothing"
   , Option ['p'] ["parent-dir"]
      (ReqArg (\d opts -> opts { optParentDir = d } ) "DIR")
      "Top-level directory where new links are created. Default: ."
   , Option ['q'] ["quiet"]
      (NoArg (\opts -> opts { optQuiet = True } ))
      "Suppress normal output of what's being done"
   , Option ['s'] ["suffix"]
      (ReqArg (\s opts -> opts { optSuffix = s } ) "SUF")
      "Add optional suffix to each name. See SUFFIX"
   , Option []    ["version"]
      (NoArg (\opts -> opts { optVersion = True } ))
      "Show version information"
   ]


{- Try to load a config file, converting its lines into a [String]
   of long options to be parsed
-}
loadConfig :: FilePath -> IO [String]
loadConfig path = do
   confExists <- doesFileExist path

   if confExists
      then (map ("--" ++) . lines) <$> readFile path
      else return []


{- Perform the actual parse of a [String]
-}
parseOpts' :: [String] -> IO (Options, [String])
parseOpts' args =
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageText)


{- The set of steps to parse args from both the command-line and a
   config file
-}
parseOpts :: [String] -> IO (Options, [String])
parseOpts cliArgs = do
   -- Parse argv first time to get -c and/or -C
   (prelimOpts, _) <- parseOpts' cliArgs

   -- Load config file
   confArgs <- if (optNoConfig prelimOpts)
      then return []
      else loadConfig $ optConfig prelimOpts

   -- Parse second time with all args
   parseOpts' (confArgs ++ cliArgs)


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: photoname [OPTIONS] FILES"
         , "Rename and move photo files based on EXIF data"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "This software is for renaming and storing your digital photos. It will attempt to construct a meaningful filename based on the EXIF shoot date in the file and optionally some other information."
         , ""
         , "FILENAME FORMAT"
         , ""
         , "Normal operation builds a subdirectory hierarchy consisting of directories for the years, then subdirs within those for the day photos were shot. These day dirs contain the image files, named as follows:"
         , ""
         , "A photo shot on 2002-May-02 01:23:07 PM:"
         , "  img_1790.jpg -> <PARENTDIR>/2002/2002-05-02/20020502-132307.jpg"
         , ""
         , "The EXIF date/time stamp used for naming is the first of these fields to be found: Exif.Photo.DateTimeDigitized, Exif.Photo.DateTimeOriginal, Exif.Image.DateTime"
         , ""
         , "The <PARENTDIR> is the one given by the --parent-dir switch and represents the top-level of where you're storing photos."
         , ""
         , "The --no-dirs switch will suppress the directory-hierarchy-creating part of this, instead placing the new links directly in <PARENTDIR>. So you get files like:"
         , ""
         , "  <PARENTDIR>/20020502-132307.jpg"
         , ""
         , "Default behavior is to create hard links to the new paths and leave the original links as they were. You can use the --move switch to not leave the original links."
         , ""
         , "SUFFIX"
         , ""
         , "The optional --suffix switch can be used to provide a string placed between the date/time and extension. Use it for anything you like. An example is photographer initials or edit info. Example:"
         , ""
         , "photoname invoked with --suffix=_dwm :"
         , "  20020502-132307_dwm.jpg"
         , ""
         , "photoname invoked with --suffix=_sd1920x1080 (Sized for Desktop 1920x1080) :"
         , "  20020502-132307_sd1920x1080.jpg"
         , ""
         , "photoname invoked with --suffix=_BirthdayParty :"
         , "  20020502-132307_BirthdayParty.jpg"
         , ""
         , "CONFIG"
         , ""
         , "The program will attempt to load a config file from this location: " ++ defaultConfig ++ "  A different path may be specified with the --config switch"
         , "Entries in this file should be the long switches above minus the -- and including any arguments they may have. To completely ignore an existing config file, use the --no-config switch."
         , ""
         , "Example config contents:"
         , ""
         , "  move"
         , "  parent-dir=~/mypics"
         , "  suffix=_dwm"
         , ""
         , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
         ]


formattedVersion :: IO String
formattedVersion = return $ "photoname " ++ (showVersion version)
