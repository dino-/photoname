module Photoname.Exiv2
  ( getExifDateWithExiv2
  , setArtist
  , setExifDate
  )
  where

import Control.Exception
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO )
import Data.Char (isSpace)
import GHC.IO.Exception
import System.Process hiding ( proc )
import qualified System.Process as Proc
import Text.Printf (printf)

import Photoname.Common ( Artist ( Artist), DestPath ( DestPath),
  NoActionSwitch ( NoActionSwitch), Options ( artist, noAction), Ph,
  SrcPath ( SrcPath), asks, liftIO )
import Photoname.Date ( PhDate ( FilenameDate), formatDateForExif )
import Photoname.Log ( LogFunction, debugM, infoM, lname, noticeM )


data Reading
data Writing

-- For logging purposes we keep the program name separate from its arguments
-- until we need to build a CreateProcess data structure
data Command rw = Command LogFunction FilePath [String]

-- Construct a human-readable command-line from a Command data structure. This
-- is purely for logging.
-- Arguments may contain spaces but be quoted properly when this is used by
-- System.Process.proc BUT they look odd when logged by our code. This function
-- will put quotes around any space-containing arguments purely for human
-- readability.
commandToString :: Command rw -> String
commandToString (Command _ program' arguments) =
  unwords $ program' : map quoteAsNeeded arguments
  where
    quoteAsNeeded str = if ' ' `elem` str
      then "'" <> str <> "'"
      else str

proc :: Command rw -> CreateProcess
proc (Command _ program' arguments) = Proc.proc program' arguments


logCommand :: MonadIO m => Command rw -> m ()
logCommand command@(Command logFunction _ _) =
  liftIO . logFunction lname . commandToString $ command



-- For Writing (or "destructive") commands, we need to check if the user has
-- chosen no-action behavior before executing
execWritingCommand :: Command Writing -> Ph (Either String String)
execWritingCommand command = do
  logCommand command
  (NoActionSwitch noAction') <- asks noAction
  if noAction'
    then pure $ Left ""
    else execCommand command


-- For Reading (or "non-destructive") commands, we just log it and do it
execReadingCommand :: Command Reading -> Ph (Either String String)
execReadingCommand command = logCommand command >> execCommand command


stripTrailingWhitespace :: String -> String
stripTrailingWhitespace = reverse . dropWhile isSpace . reverse


execCommand :: Command rw -> Ph (Either String String)
execCommand command = liftIO $ do
  eResult <- postProcess =<< try (readCreateProcessWithExitCode (proc command) "")
  either (\msg -> debugM lname $ "** Command failed: " <> stripTrailingWhitespace msg)
    (\output -> debugM lname $ "Command succeeded, output: " <> stripTrailingWhitespace output) eResult
  pure eResult


program :: FilePath
program = "exiv2"


postProcess :: Either IOException (ExitCode, String, String) -> IO (Either String String)
postProcess (Left   e                             ) = pure . Left $ printf "%s: %s" program (ioe_description e)
postProcess (Right (ExitSuccess  , stdOut, _     )) = pure . Right $ stdOut
postProcess (Right (ExitFailure 1, _     , ""    )) = pure . Left $ "EXIF tag not found"
postProcess (Right (ExitFailure _, _     , stdErr)) = pure . Left $ stdErr


setArtist :: DestPath -> Ph ()
setArtist (DestPath destFp) = do
  artist' <- asks artist

  case artist' of
    Nothing -> pure ()
    Just (Artist "") -> void $ execWritingCommand $
      Command noticeM program ["--Modify", "del Exif.Image.Artist", destFp]
    Just (Artist artistInfo) -> void $ execWritingCommand $
      Command noticeM program ["--Modify", "set Exif.Image.Artist " <> artistInfo, destFp]


getExifDateWithExiv2 :: SrcPath -> Ph (Maybe String)
getExifDateWithExiv2 (SrcPath srcFp) = do
  let tag = "Exif.Image.DateTime"
  eResult <- execReadingCommand $ Command infoM program ["--Print", "v", "--grep", tag, srcFp]
  pure . either (const Nothing) Just $ eResult


setExifDate :: PhDate -> DestPath -> Ph ()

setExifDate (FilenameDate lt) (DestPath destFp) =
  void $ execWritingCommand . Command noticeM program $
    [ "--Modify", "set Exif.Image.DateTime Ascii " <> formatDateForExif lt
    , "--Modify", "set Exif.Photo.UserComment charset=Ascii DateTime is a guess", destFp
    ]

setExifDate _ _ = pure ()
