{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TestLink
  ( tests
  )
  where

import System.Directory ( copyFile, removeDirectoryRecursive, removeFile )
import System.FilePath.Posix ( (</>), (<.>) )
import System.Posix.Files ( fileExist )
import System.Process ( waitForProcess )
import Text.Regex.Posix ( (=~) )
import Test.Tasty
import Test.Tasty.HUnit
import qualified Util


parentDir, oldPath, newLinkPathDate :: FilePath

parentDir = Util.resourcesPath </> "testParentDir"
oldPath = Util.resourcesPath </> "dateTimeDigitized.jpg"
newLinkPathDate = parentDir </> "2003/2003-09-02/20030902-114303.jpg"


tests :: TestTree
tests = testGroup "test the normal behavior of hard-linking original files to new paths"
  [ testLinkDigitized
  , testLinkOriginal
  , testLinkDate
  , testNoDate
  , testMove
  , testLinkNoAction
  , testLinkNoActionLong
  , testLinkQuiet
  , testLinkQuietLong
  , testLinkSuffix
  , testNoExif
  , testNotAnImage
  , testDirForFile
  , testLinkFilenameDate
  ]


testLinkDigitized :: TestTree
testLinkDigitized = testCase "tests for DateTimeDigitized" $ do
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


testLinkOriginal :: TestTree
testLinkOriginal = testCase "tests for DateTimeOriginal" $ do
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


testLinkDate :: TestTree
testLinkDate = testCase "tests for DateTime" $ do
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


testNoDate :: TestTree
testNoDate = testCase "test for no date in the file" $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, Util.resourcesPath </> "noDate.jpg" ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "no EXIF: correct output"
      (output =~ "\\*\\* Processing util/resources/test/noDate.jpg: Could not extract any date information" :: Bool)


testMove :: TestTree
testMove = testCase "tests to ensure the file is moved (original link removed)" $ do
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


testLinkNoAction :: TestTree
testLinkNoAction = testLinkNoAction' "no action" "-n"


testLinkNoActionLong :: TestTree
testLinkNoActionLong = testLinkNoAction' "no action long" "--no-action"


testLinkNoAction' :: String -> String -> TestTree
testLinkNoAction' label switch = testCase label $ do
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


testLinkQuiet :: TestTree
testLinkQuiet = testLinkQuiet' "make link quiet" "-q"


testLinkQuietLong :: TestTree
testLinkQuietLong = testLinkQuiet' "make link quiet long" "--quiet"


-- Reusable test code for above short/long versions of the quiet switch
testLinkQuiet' :: String -> String -> TestTree
testLinkQuiet' label switch = testCase label $ do
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


testLinkSuffix :: TestTree
testLinkSuffix = testCase "test link with a suffix" $ do
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


testNoExif :: TestTree
testNoExif = testCase "test for a file without any EXIF data" $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, Util.resourcesPath </> "noExif.jpg" ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "no EXIF: correct output"
      (output =~ "\\*\\* Processing util/resources/test/noExif.jpg: Could not extract any date information" :: Bool)


testNotAnImage :: TestTree
testNotAnImage = testCase "test for a file that isn't an image" $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, Util.resourcesPath </> "notAnImage.txt" ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "no EXIF: correct output"
      (output =~ "\\*\\* Processing util/resources/test/notAnImage.txt: Could not extract any date information" :: Bool)


testDirForFile :: TestTree
testDirForFile = testCase "test when a directory is passed instead of a file" $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, Util.resourcesPath ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "dir as file to change: correct output"
      (output =~ "" :: Bool)


testLinkFilenameDate :: TestTree
testLinkFilenameDate = testCase "test to ensure the date can be acquired from the file name" $ do
   -- Make a dummy copy of the source file. This test will be modifying
   -- it, if successful.
   let newOldPath = Util.resourcesPath </> "copy-of-foobar-2021-10-04-17-29-49-942.jpg"
   copyFile (Util.resourcesPath </> "foobar-2021-10-04-17-29-49-942.jpg") newOldPath
   let newLinkPathDate' = parentDir </> "2021/2021-10-04/20211004-172949.jpg"

   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ parentDir, newOldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPathDate'
   assertBool "filename date: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist newOldPath
   assertBool "filename date: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeFile newOldPath
   removeDirectoryRecursive parentDir

   -- WARNING We are NOT checking for the new EXIF tags in the files!!

   -- Test output to stdout
   assertBool "filename date: correct output"
      (output =~ newLinkPathDate' :: Bool)
