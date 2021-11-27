module Util
  ( resourcesPath
  , getProcessOutput, getBinaryOutput
  , assertFalse
  )
  where

import System.IO ( hGetContents )
import System.Process ( ProcessHandle, runInteractiveCommand )
import Test.Tasty.HUnit ( Assertion, assertBool )


command :: String
command = "stack exec photoname"


resourcesPath :: FilePath
resourcesPath = "util/resources/test"


{- Quick and dirty function to run a process and grab its output.
   This evil thing doesn't watch STDERR at all or otherwise do anything
   even remotely safe.
-}
getProcessOutput :: String -> [String] -> IO (String, ProcessHandle)
getProcessOutput path' args = do
   (_, outH, _, procH) <- runInteractiveCommand
      $ path' <> " -- " <> unwords args
   output <- hGetContents outH
   pure (output, procH)


getBinaryOutput :: [String] -> IO (String, ProcessHandle)
getBinaryOutput = getProcessOutput command


assertFalse :: String -> Bool -> Assertion
assertFalse l b = assertBool l $ not b
