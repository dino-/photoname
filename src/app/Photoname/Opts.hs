{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Photoname.Opts
   ( parseOpts
   )
   where

import Data.Version ( showVersion )
import Options.Applicative
import Paths_photoname ( version )
import System.Directory ( doesFileExist )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import System.Log ( Priority (INFO) )
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )
import Text.Printf ( printf )

import Photoname.Common
  ( Artist (..)
  , ConfigPath (..)
  , CopySwitch (..)
  , Extension (Extension, UseExistingExtension)
  , Links (Exactly, NoLimit)
  , MoveSwitch (..)
  , NoActionSwitch (..)
  , NoDirsSwitch (..)
  , Options (..)
  , ParentDir (..)
  , Prefix (..)
  , Suffix (..)
  , Verbosity (Verbose)
  , defaultDateTimeFormat
  , readVerbosity
  )
import Photoname.Date (formatDateTime, mkDateFormatter)


parser :: Parser Options
parser = Options
  <$> optional ( option (maybeReader $ Just . Artist)
        (  long "artist"
        <> short 'a'
        <> metavar "ARTIST"
        <> help "Set artist info in the Exif.Image.Artist tag. Requires exiv2. See ARTIST"
        )
      )
  <*> optional ( option (maybeReader $ Just . ConfigPath)
        (  long "config"
        <> short 'c'
        <> metavar "FILE"
        <> help "Path to a config file. See CONFIG"
        )
      )
  <*> ( CopySwitch <$> switch
        (  long "copy"
        <> help "Copy files instead of hard linking, even if on the same filesystem"
        )
      )
  <*> ( NoDirsSwitch <$> switch
        (  long "no-dirs"
        <> short 'D'
        <> help "No subdirectory hierarchy. Just do DIR/NEWFILE"
        )
      )
  <*> option (Extension <$> str)
        (  long "extension"
        <> short 'e'
        <> metavar "EXT"
        <> help "Extension to use for new image file names. Default: Keep existing extension"
        <> value UseExistingExtension
        )
  <*> option (mkDateFormatter <$> str)
        (  long "date-formatter"
        <> short 'f'
        <> metavar "STR"
        <> help (printf "Format string for date/time in the new filenames. Default \"%s\" See DATE FORMATTER"
            defaultDateTimeFormat)
        <> value formatDateTime
        )
  <*> option (Exactly <$> auto)
        (  long "links"
        <> short 'l'
        <> metavar "NUM"
        <> help "Reject files unless they have this many hard links. See LINKS. Default: accept all files"
        <> value NoLimit
        )
  <*> ( MoveSwitch <$> switch
        (  long "move"
        <> help "Move the files, don't just hard-link to the new locations. In other words, remove the source path."
        )
      )
  <*> ( NoActionSwitch <$> switch
        (  long "no-action"
        <> short 'n'
        <> help "Display what would be done, but do nothing"
        )
      )
  <*> ( ParentDir <$> strOption
        (  long "parent-dir"
        <> short 'p'
        <> metavar "DIR"
        <> help "Top-level directory where new links are created."
        <> showDefault
        <> value "."
        )
      )
  <*> ( Prefix <$> strOption
        (  long "prefix"
        <> short 'P'
        <> metavar "PRE"
        <> help "Add optional prefix to each name. See PREFIX"
        <> value ""
        )
      )
  <*> ( Suffix <$> strOption
        (  long "suffix"
        <> short 's'
        <> metavar "SUF"
        <> help "Add optional suffix to each name. See SUFFIX"
        <> value ""
        )
      )
  <*> option (eitherReader readVerbosity)
        (  long "verbose"
        <> short 'v'
        <> metavar "NUM"
        <> help "Verbosity level. 0=quiet, 1=normal messages, 2=more info, 3=debug"
        <> showDefault
        <> value (Verbose INFO)
        )
  <*> some ( strArgument
        $ metavar "FILES..."
      )


{- Try to load a config file, converting its lines into a [String]
   of long options to be parsed
-}
loadConfig :: ConfigPath -> IO [String]
loadConfig (ConfigPath path) = do
   confExists <- doesFileExist path

   if confExists
      then map ("--" ++) . lines <$> readFile path
      else do
        hPutStrLn stderr $ "Config file " <> path <> " does not exist!"
        exitFailure


versionHelper :: String -> Parser (a -> a)
versionHelper progName =
  infoOption (printf "%s %s" progName (showVersion version)) $ mconcat
  [ long "version"
  , help "Show version information"
  , hidden
  ]


parseOpts :: IO Options
parseOpts = do
  -- Parse command-line args first to get -c
  cliOpts <- parseOpts' =<< getArgs

  case config cliOpts of
    Just configPath -> do
      confArgs <- loadConfig configPath
      parseOpts' $ confArgs <> paths cliOpts
    Nothing -> pure cliOpts


parseOpts' :: [String] -> IO Options
parseOpts' args = do
  pn <- getProgName
  handleParseResult $ execParserPure
    defaultPrefs
    ( info (parser <**> helper <**> versionHelper pn)
      ( header (printf "%s - Rename and move photo files based on EXIF data" pn)
      <> footer'
      )
    )
    args


footer' :: InfoMod a
footer' = footerDoc . Just . string $ printf content defaultDateTimeFormat (showVersion version)
  where content = [here|OVERVIEW

This software is for renaming and storing your digital photos. It will attempt to construct a meaningful filename based on the EXIF shoot date in the file or possibly date/time info in the old filename, and optionally some other information.

FILENAME FORMAT

Normal operation builds a subdirectory hierarchy consisting of directories for the years, then subdirs within those for the day photos were shot. These day dirs contain the image files, named as follows:

A photo shot on 2002-May-02 01:23:07 PM:
  img_1790.jpg -> <PARENTDIR>/2002/2002-05-02/20020502-132307.jpg

The EXIF date/time stamp used for naming is the first of these fields to be found: Exif.Photo.DateTimeOriginal, Exif.Photo.DateTimeDigitized, Exif.Image.DateTime

If none of the EXIF tags listed above is found, the program will try to gather date/time info from the filename itself. Filenames that are parsable look like:

  some/directory/foo2021-10-04-172949.jpg
  some/directory/foo2021-10-04-17-29-49-942.jpg

In the event the date/time info is gathered from the filename, the program will go ahead and write this into the file's EXIF tags. BEWARE: Unless you're using the --copy switch, this WILL MODIFY THE ORIGINAL FILES!

The EXIF modifications rely on the system having the `exiv2` binary installed and on the path. Without this, some of photoname's functions will fail including: setting the date from the filename and setting the artist info.

The <PARENTDIR> is the one given by the -p|--parent-dir switch and represents the top-level of where you're storing photos.

The -D|--no-dirs switch will suppress the directory-hierarchy-creating part of this, instead placing the new links directly in <PARENTDIR>. So you get files like:

  <PARENTDIR>/20020502-132307.jpg

Default behavior is to create hard links to the new paths and leave the original links as they were. You can use the --move switch to remove the original links. You can also use the --copy switch to make a copy instead of hard linking. Also, copying will be attempted if the hard linking fails, for instance if you're naming across different filesystems.

DATE FORMATTER

The default date format string is "%s" which yields strings like "yyyymmdd-HHMMSS"
Another example: "%%Y-%%m-%%dt%%H%%M_img" -> "yyyy-mm-ddtHHMM_img"

Any valid date format string is allowed for the -f|--date-formatter option. Please see the API documentation for help on format strings: https://downloads.haskell.org/ghc/latest/docs/libraries/time-1.12.2-dfcf/Data-Time-Format.html

ARTIST

Set artist info in the Exif.Image.Artist tag. This can be any string you like but good conventional ones would look like:

  'Role1, Name1[; Role2, Name2;...]'

  'Photographer, Roscoe Jones'
  'Camera owner, Ren Hoek; Photographer, Stimpson J Cat'

Be careful with what you put in here, we've seen problems with email addresses rendering the entire field not visible in some applications. Keep it simple as above!

Pass a quoted empty string to -a|--artist to delete an existing Artist tag, like this: -a '' or --artist=''

LINKS

A common use of photoname is to process a directory of images, leaving you with 2 hard links to the original files. If at a later time you want to process any new images in the same directory, they could be identified by their different number of links. The -l|--links switch can perform this filtering in lieu of using another tool like `find`.

PREFIX

The optional -P|--prefix switch can be used to provide a string placed at the beginning of the new filename, before the date. Use it for anything you like. An example is subject matter. Example:

photoname invoked with --prefix=BirthdayParty_ :
  BirthdayParty_20020502-132307.jpg

SUFFIX

The optional -s|--suffix switch can be used to provide a string placed between the date/time and extension. Use it for anything you like. An example is photographer initials or edit info. Example:

photoname invoked with --suffix=_dwm :
  20020502-132307_dwm.jpg

photoname invoked with --suffix=_sd1920x1080 (Sized for Desktop 1920x1080) :
  20020502-132307_sd1920x1080.jpg

photoname invoked with --suffix=_BirthdayParty :
  20020502-132307_BirthdayParty.jpg

CONFIG

If the -c|--config switch is used the program will attempt to load a config file from the supplied path
Entries in this file should be the long switches above minus the -- and including any arguments they may have. If a config file is used, all other command-line switches are ignored.

Example config contents:

  move
  parent-dir=~/mypics
  suffix=_dwm

Version %s  Dino Morelli <dino@ui3.info>|]
