{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TestLink (
   testLinkAll
)
   where

import System.Directory ( copyFile, removeDirectoryRecursive )
import System.FilePath.Posix ( (</>), (<.>) )
import System.Posix.Files ( fileExist )
import System.Process ( waitForProcess )
import Test.HUnit ( Test (..), assertBool )
import Text.Regex.Posix ( (=~) )
import qualified Util


{- Test the normal behavior of hard-linking the original file to a new
   path.
-}


testLinkAll :: Test
testLinkAll = TestList
   [ TestLabel "testLinkDigitized" testLinkDigitized
   , TestLabel "testLinkOriginal" testLinkOriginal
   , TestLabel "testLinkDate" testLinkDate
   , TestLabel "testNoDate" testNoDate
   , TestLabel "testMove" testMove
   , TestLabel "testLinkNoAction" testLinkNoAction
   , TestLabel "testLinkNoActionLong" testLinkNoActionLong
   , TestLabel "testLinkQuiet" testLinkQuiet
   , TestLabel "testLinkQuietLong" testLinkQuietLong
   , TestLabel "testLinkSuffix" testLinkSuffix
   , TestLabel "testNoExif" testNoExif
   , TestLabel "testNotAnImage" testNotAnImage
   , TestLabel "testDirForFile" testDirForFile
   ]


parentDir, oldPath, newLinkPathDate :: FilePath

parentDir = Util.resourcesPath </> "testParentDir"
oldPath = Util.resourcesPath </> "dateTimeDigitized.jpg"
newLinkPathDate = parentDir </> "2003/2003-09-02/20030902-114303.jpg"


testLinkDigitized :: Test
testLinkDigitized = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, oldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPathDate
   assertBool "make link, date digitized: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool "make link, date digitized: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive parentDir

   -- Test output to stdout
   assertBool "make link, date digitized: correct output"
      (output =~ newLinkPathDate :: Bool)


testLinkOriginal :: Test
testLinkOriginal = TestCase $ do
   let digitizedOldPath = Util.resourcesPath </> "dateTimeOriginal.jpg"

   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, digitizedOldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPathDate
   assertBool "make link, date original: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist digitizedOldPath
   assertBool "make link, date original: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive parentDir

   -- Test output to stdout
   assertBool "make link, date original: correct output"
      (output =~ newLinkPathDate :: Bool)


testLinkDate :: Test
testLinkDate = TestCase $ do
   let dateOldPath = Util.resourcesPath </> "dateTime.jpg"
   let customLinkPathDate = parentDir </> "2019/2019-03-26/20190326-075309.jpg"

   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, dateOldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist customLinkPathDate
   assertBool "make link, date: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist dateOldPath
   assertBool "make link, date: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive parentDir

   -- Test output to stdout
   assertBool "make link, date: correct output"
      (output =~ customLinkPathDate :: Bool)


testNoDate :: Test
testNoDate = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, Util.resourcesPath </> "noDate.jpg" ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "no EXIF: correct output"
      (output =~ "\\*\\* Processing util/resources/test/noDate.jpg: No dates found in EXIF data" :: Bool)


testMove :: Test
testMove = TestCase $ do
   -- Make a dummy copy of the source file. This test will be getting rid
   -- of it, if successful.
   let newOldPath = Util.resourcesPath </> "moveTest.jpg"
   copyFile oldPath newOldPath

   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--move", "--parent-dir=" ++ parentDir, newOldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPathDate
   assertBool "move file: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist newOldPath
   Util.assertFalse "move file: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive parentDir

   -- Test output to stdout
   assertBool "move file: correct output"
      (output =~ newLinkPathDate :: Bool)


testLinkNoAction :: Test
testLinkNoAction = testLinkNoAction' "no action" "-n"


testLinkNoActionLong :: Test
testLinkNoActionLong = testLinkNoAction' "no action long" "--no-action"


testLinkNoAction' :: String -> String -> Test
testLinkNoAction' label switch = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ switch, "--parent-dir=" ++ parentDir, oldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist parentDir
   Util.assertFalse (label ++ ": existance of new link") existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool (label ++ ": existance of old link") existsOld

   -- Test output to stdout
   assertBool (label ++ ": correct output")
      (output =~ newLinkPathDate :: Bool)


testLinkQuiet :: Test
testLinkQuiet = testLinkQuiet' "make link quiet" "-q"


testLinkQuietLong :: Test
testLinkQuietLong = testLinkQuiet' "make link quiet long" "--quiet"


-- Reusable test code for above short/long versions of the quiet switch
testLinkQuiet' :: String -> String -> Test
testLinkQuiet' label switch = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ switch, "--parent-dir=" ++ parentDir, oldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPathDate
   assertBool (label ++ ": existance of new link") existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool (label ++ ": existance of old link") existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive parentDir

   -- Test output to stdout
   Util.assertFalse (label ++ ": no output")
      (output =~ newLinkPathDate :: Bool)


testLinkSuffix :: Test
testLinkSuffix = TestCase $ do
   let suffix = "_dwm"

   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, "--suffix=" ++ suffix, oldPath ]
   waitForProcess procH

   let newLinkPath = parentDir </>
         ("2003/2003-09-02/20030902-114303" ++ suffix) <.> "jpg"

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPath
   assertBool "make link: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool "make link: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive parentDir

   -- Test output to stdout
   assertBool "make link: correct output"
      (output =~ newLinkPath :: Bool)


testNoExif :: Test
testNoExif = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, Util.resourcesPath </> "noExif.jpg" ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "no EXIF: correct output"
      (output =~ "\\*\\* Processing util/resources/test/noExif.jpg: No EXIF in JPEG" :: Bool)


testNotAnImage :: Test
testNotAnImage = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, Util.resourcesPath </> "notAnImage.txt" ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "no EXIF: correct output"
      (output =~ "\\*\\* Processing util/resources/test/notAnImage.txt: Not a JPEG, TIFF, RAF, or TIFF-based raw file" :: Bool)


testDirForFile :: Test
testDirForFile = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, Util.resourcesPath ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "dir as file to change: correct output"
      (output =~ "" :: Bool)
