-- Copyright: 2007-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

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
   [ TestLabel "testLinkDate" testLinkDate
   , TestLabel "testLinkSerial" testLinkSerial
   , TestLabel "testMove" testMove
   , TestLabel "testLinkNoAction" testLinkNoAction
   , TestLabel "testLinkNoActionLong" testLinkNoActionLong
   , TestLabel "testLinkQuiet" testLinkQuiet
   , TestLabel "testLinkQuietLong" testLinkQuietLong
   , TestLabel "testLinkSuffix" testLinkSuffix
   , TestLabel "testNoExif" testNoExif
   , TestLabel "testNoSerial" testNoSerial
   , TestLabel "testDirForFile" testDirForFile
   ]


topDir, oldPath, newLinkPathDate, newLinkPathSerial :: FilePath

topDir = Util.resourcesPath </> "foo"
oldPath = Util.resourcesPath </> "img_1220.jpg"
newLinkPathDate = topDir </> "2003/2003-09-02/20030902-114303.jpg"
newLinkPathSerial = topDir </> "2003/2003-09-02/20030902_220.jpg"


testLinkDate :: Test
testLinkDate = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ topDir, oldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPathDate
   assertBool "make link: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool "make link: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive topDir

   -- Test output to stdout
   assertBool "make link: correct output"
      (output =~ newLinkPathDate :: Bool)


testLinkSerial :: Test
testLinkSerial = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ topDir, "--old-style", oldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPathSerial
   assertBool "make link: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool "make link: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive topDir

   -- Test output to stdout
   assertBool "make link: correct output"
      (output =~ newLinkPathSerial :: Bool)


testMove :: Test
testMove = TestCase $ do
   let newNewLinkPath = topDir </> "2003/2003-09-02/20030902_321.jpg"

   -- Make a dummy copy of the source file. This test will be getting rid
   -- of it, if successful.
   let newOldPath = Util.resourcesPath </> "img_0321.jpg"
   copyFile oldPath newOldPath

   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--move", "--parent-dir=" ++ topDir, "--old-style", newOldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newNewLinkPath
   assertBool "move file: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist newOldPath
   Util.assertFalse "move file: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive topDir

   -- Test output to stdout
   assertBool "move file: correct output"
      (output =~ newNewLinkPath :: Bool)


testLinkNoAction :: Test
testLinkNoAction = testLinkNoAction' "no action" "-n"


testLinkNoActionLong :: Test
testLinkNoActionLong = testLinkNoAction' "no action long" "--no-action"


testLinkNoAction' :: String -> String -> Test
testLinkNoAction' label switch = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ switch, "--parent-dir=" ++ topDir, "--old-style", oldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist topDir
   Util.assertFalse (label ++ ": existance of new link") existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool (label ++ ": existance of old link") existsOld

   -- Test output to stdout
   assertBool (label ++ ": correct output")
      (output =~ newLinkPathSerial :: Bool)


testLinkQuiet :: Test
testLinkQuiet = testLinkQuiet' "make link quiet" "-q"


testLinkQuietLong :: Test
testLinkQuietLong = testLinkQuiet' "make link quiet long" "--quiet"


-- Reusable test code for above short/long versions of the quiet switch
testLinkQuiet' :: String -> String -> Test
testLinkQuiet' label switch = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ switch, "--parent-dir=" ++ topDir, "--old-style", oldPath ]
   waitForProcess procH

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPathSerial
   assertBool (label ++ ": existance of new link") existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool (label ++ ": existance of old link") existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive topDir

   -- Test output to stdout
   Util.assertFalse (label ++ ": no output")
      (output =~ newLinkPathSerial :: Bool)


testLinkSuffix :: Test
testLinkSuffix = TestCase $ do
   let suffix = "_dwm"

   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ topDir, "--suffix=" ++ suffix, oldPath ]
   waitForProcess procH

   let newLinkPath = topDir </>
         ("2003/2003-09-02/20030902-114303" ++ suffix) <.> "jpg"

   -- Check that the correct output path exists
   existsNew <- fileExist newLinkPath
   assertBool "make link: existance of new link" existsNew

   -- Check that old path still exists
   existsOld <- fileExist oldPath
   assertBool "make link: existance of old link" existsOld

   -- Remove files and dirs that were created
   removeDirectoryRecursive topDir

   -- Test output to stdout
   assertBool "make link: correct output"
      (output =~ newLinkPath :: Bool)


testNoExif :: Test
testNoExif = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ topDir, Util.resourcesPath </> "noExif.jpg" ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "no EXIF: correct output"
      (output =~ "\\*\\* Processing testsuite/resources/noExif.jpg: Failed EXIF loading" :: Bool)


testNoSerial :: Test
testNoSerial = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ topDir
      , "--old-style", Util.resourcesPath </> "noSerial.jpg" ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "no serial in filename: correct output"
      (output =~ "\\*\\* Processing testsuite/resources/noSerial.jpg: Can't determine serial" :: Bool)


testDirForFile :: Test
testDirForFile = TestCase $ do
   -- Run the program with known input data
   (output, procH) <- Util.getBinaryOutput
      [ "--parent-dir=" ++ topDir, Util.resourcesPath ]
   waitForProcess procH

   -- Test output to stdout
   assertBool "dir as file to change: correct output"
      (output =~ "" :: Bool)
