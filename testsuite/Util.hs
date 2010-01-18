-- Copyright: 2007-2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Util (
   binPath, resourcesPath,
   getProcessOutput, getBinaryOutput,
   assertFalse
)
   where

import System.IO
import System.Process
import Test.HUnit


binPath :: FilePath
binPath = "dist/build/photoname/photoname"


resourcesPath :: FilePath
resourcesPath = "testsuite/resources"


{- Quick and dirty function to run a process and grab its output.
   This evil thing doesn't watch STDERR at all or otherwise do anything
   even remotely safe.
   XXX Move this somewhere logical like Photoname.Util
-}
getProcessOutput :: FilePath -> [String] -> IO (String, ProcessHandle)
getProcessOutput path args = do
   (_, outH, _, procH) <- runInteractiveProcess path args Nothing Nothing
   output <- hGetContents outH
   return (output, procH)


getBinaryOutput :: [String] -> IO (String, ProcessHandle)
getBinaryOutput = getProcessOutput binPath


assertFalse :: String -> Bool -> Assertion
assertFalse l b = assertBool l $ not b
