module Main
   where

import System.Exit ( ExitCode (ExitFailure, ExitSuccess), exitWith )
import Test.HUnit ( Counts (..), Test (..), runTestTT )
import TestHelp ( testHelpAll )
import TestLink ( testLinkAll )


main :: IO ()
main = do
   counts <- runTestTT tests
   exit $ testsPassed counts


exit :: Bool -> IO ()
exit True  = exitWith ExitSuccess
exit False = exitWith $ ExitFailure 1


testsPassed :: Counts -> Bool
testsPassed (Counts _ _ e f) = (e == 0) && (f == 0)


tests :: Test
tests = TestList
   [ TestLabel "testHelpAll" testHelpAll
   , TestLabel "testLinkAll" testLinkAll
   ]
