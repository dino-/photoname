module Photoname.Exiv2
  ( getExifDateWithExiv2
  , setArtist
  , setExifDate
  )
  where

import Control.Exception
-- import Control.Monad ( unless, void )
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO )
import Control.Newtype.Generics ( op )
import GHC.IO.Exception
import System.Exit
import System.Process hiding ( proc )
-- import System.Process ( callCommand )
import qualified System.Process as Proc
-- import Text.Printf ( printf )

-- import Photoname.Common ( Artist (..), DestPath (..), NoActionSwitch (..),
--   Options (..), Ph, asks, liftIO )
import Photoname.Common
import Photoname.Date ( PhDate (FilenameDate), formatDateForExif )
import Photoname.Log ( debugM, lname, noticeM )

-- import Debug.Trace


data Reading
data Writing

-- newtype Command = Command { unCommand :: String }

-- For logging purposes we keep the program name separate from its arguments
-- until we need to build a CreateProcess data structure
data Command rw = Command FilePath [String]

-- Construct a human-readable command-line from a Command data structure. This
-- is purely for logging.
commandToString :: Command rw -> String
commandToString (Command program' arguments) =
  unwords $ program' : arguments

proc :: Command rw -> CreateProcess
proc (Command program' arguments) = Proc.proc program' arguments


-- execCommand :: Command -> Ph (Either String String)
-- execCommand command = do
--   noAction <- asks $ op NoActionSwitch . optNoAction

--   -- Display what will be done
--   liftIO . noticeM lname . commandToString $ command
--   liftIO . debugM lname . show . proc $ command  -- FIXME

--   -- liftIO $ unless noAction $ do
--   --   eResult <- postProcess =<< try (readCreateProcessWithExitCode (proc command) "")
--   --   either (debugM lname) (debugM lname) eResult

--   liftIO $ if noAction
--     then pure $ Left ""
--     else do
--       -- eResult <- postProcess =<< try (readCreateProcessWithExitCode (proc command) "")
--       eResult <- (postProcess . traceShowId) =<< try (readCreateProcessWithExitCode (proc command) "")
--       either (debugM lname) (debugM lname) eResult
--       pure eResult


logCommand :: Control.Monad.IO.Class.MonadIO m => Command rw -> m ()
logCommand command = liftIO . noticeM lname . commandToString $ command



-- For Writing (or "destructive") commands, we need to check if the user has
-- chosen no-action behavior before executing
execWritingCommand :: Command Writing -> Ph (Either String String)
execWritingCommand command = do
  logCommand command
  noAction <- asks $ op NoActionSwitch . optNoAction
  if noAction
    then pure $ Left ""
    else execCommand command


-- For Reading (no "non-destructive") commands, we just log it and do it
execReadingCommand :: Command Reading -> Ph (Either String String)
execReadingCommand command = logCommand command >> execCommand command


execCommand :: Command rw -> Ph (Either String String)
execCommand command = liftIO $ do
  eResult <- postProcess =<< try (readCreateProcessWithExitCode (proc command) "")
  either (debugM lname) (debugM lname) eResult
  pure eResult


program :: FilePath
program = "exiv2"


postProcess :: (Either IOException (ExitCode, String, String)) -> IO (Either String String)
postProcess (Left   e                             ) = pure . Left $ ("exiv2: " <> ioe_description e)
postProcess (Right (ExitSuccess  , stdOut, _     )) = pure . Right $ stdOut
postProcess (Right (ExitFailure 1, _     , ""    )) = pure . Left $ "EXIF tag not found"
postProcess (Right (ExitFailure _, _     , stdErr)) = pure . Left $ stdErr


setArtist :: DestPath -> Ph ()
setArtist (DestPath destFp) = do
  artist <- asks optArtist

  case artist of
    Nothing -> pure ()
    Just (Artist "") -> void $ execWritingCommand $ Command program ["--Modify", "del Exif.Image.Artist", destFp]
    Just (Artist artistInfo) -> void $ execWritingCommand $ Command program ["--Modify", "set Exif.Image.Artist " <> artistInfo, destFp]


getExifDateWithExiv2 :: SrcPath -> Ph (Maybe String)
getExifDateWithExiv2 (SrcPath srcFp) = do
  let tag = "Exif.Image.DateTime"
  eResult <- execReadingCommand $ Command program ["--Print", "v", "--grep", tag, srcFp]
  pure . either (const Nothing) Just $ eResult


setExifDate :: PhDate -> DestPath -> Ph ()

setExifDate (FilenameDate lt) (DestPath destFp) =
  void $ execWritingCommand . Command program $
    [ "--Modify", "set Exif.Image.DateTime Ascii " <> (formatDateForExif lt)
    , "--Modify", "set Exif.Photo.UserComment charset=Ascii DateTime is a guess", destFp
    ]

setExifDate _ _ = pure ()
      



-- getWithExiv2 :: FilePath -> Tag -> IO (Either String String)
-- getWithExiv2 filePath (Tag tag) = try action >>= postProcess
--   where
--     -- This is for the helpful error message if the program is not installed
--     program :: String
--     program = "exiv2"

--     action :: IO (ExitCode, String, String)
--     action = readProcessWithExitCode program ["--Print", "v", "--grep", tag, filePath] ""

--     -- try evaluates to an Either but we want one with different types and we
--     -- also want some constructive error messages from this function
--     postProcess :: (Either IOException (ExitCode, String, String)) -> IO (Either String String)
--     postProcess (Left   e                             ) = pure . Left $ (program <> ": " <> ioe_description e)
--     postProcess (Right (ExitSuccess  , stdOut, _     )) = pure . Right $ stdOut
--     postProcess (Right (ExitFailure 1, _     , ""    )) = pure . Left $ "Tag not found: " <> tag
--     postProcess (Right (ExitFailure _, _     , stdErr)) = pure . Left $ stdErr


-- execCommands :: [Command] -> Ph ()
-- execCommands commands = do
--   opts <- ask

--   -- Display what will be done
--   liftIO $ mapM_ (noticeM lname . unCommand) commands

--   -- Execute the commands
--   unless (op NoActionSwitch . optNoAction $ opts) $
--     liftIO $ mapM_ (callCommand . unCommand) commands


-- setArtist :: DestPath -> Ph ()
-- setArtist (DestPath destFp) = do
--   opts <- ask

--   case optArtist opts of
--     Nothing -> pure ()
--     Just (Artist "") -> execCommands . map Command $
--       [ printf "exiv2 --Modify 'del Exif.Image.Artist' %s" destFp ]
--     Just (Artist artistInfo) -> execCommands . map Command $
--       [ printf "exiv2 --Modify 'set Exif.Image.Artist %s' %s" artistInfo destFp ]


-- setExifDate :: PhDate -> DestPath -> Ph ()

-- setExifDate (FilenameDate lt) (DestPath destFp) =
--   execCommands . map Command $
--     [ printf "exiv2 --Modify 'set Exif.Image.DateTime Ascii %s' --Modify 'set Exif.Photo.UserComment charset=Ascii DateTime is a guess' %s" (formatDateForExif lt) destFp
--     ]

-- setExifDate _ _ = pure ()
