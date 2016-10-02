-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Util (
   resourcesPath,
   getProcessOutput, getBinaryOutput,
   assertFalse
)
   where

import Data.List ( intercalate )
import System.IO ( hGetContents )
import System.Process ( ProcessHandle, runInteractiveCommand )
import Test.HUnit ( Assertion, assertBool )


command :: String
command = "stack exec photoname"


resourcesPath :: FilePath
resourcesPath = "testsuite/resources"


{- Quick and dirty function to run a process and grab its output.
   This evil thing doesn't watch STDERR at all or otherwise do anything
   even remotely safe.
-}
getProcessOutput :: FilePath -> [String] -> IO (String, ProcessHandle)
getProcessOutput path' args = do
   (_, outH, _, procH) <- runInteractiveCommand
      $ path' ++ " -- " ++ (intercalate " " args)
   output <- hGetContents outH
   return (output, procH)


getBinaryOutput :: [String] -> IO (String, ProcessHandle)
getBinaryOutput = getProcessOutput command


assertFalse :: String -> Bool -> Assertion
assertFalse l b = assertBool l $ not b
