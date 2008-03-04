-- Copyright: 2007, 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Main
   where

import Test.HUnit
import TestHelp
import TestLink


main :: IO Counts
main = runTestTT tests


tests :: Test
tests = TestList
   [ TestLabel "testHelpAll" testHelpAll
   , TestLabel "testLinkAll" testLinkAll
   ]
