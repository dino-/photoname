-- Copyright: 2007-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Main
   where

import System.Exit
import Test.HUnit ( Counts (..), Test (..), runTestTT )
import TestHelp
import TestLink


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
