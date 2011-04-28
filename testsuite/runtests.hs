-- Copyright: 2007-2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Main
   where

import Test.HUnit
import TestHelp
import TestLink


main :: IO ()
main = do
   runTestTT tests
   return ()


tests :: Test
tests = TestList
   [ TestLabel "testHelpAll" testHelpAll
   , TestLabel "testLinkAll" testLinkAll
   ]
