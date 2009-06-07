-- Copyright: 2007-2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module TestHelp (
   testHelpAll
)
   where

import qualified Util
import Test.HUnit ( Test (..), assertBool, assertEqual )


testHelpAll :: Test
testHelpAll = TestList
   [ TestLabel "testHelpSwitch" testHelpSwitch
   , TestLabel "testHelpSwitchLong" testHelpSwitchLong
   ]

expectedFirstLine :: String
expectedFirstLine = "Usage: photoname [OPTIONS] PARENTDIR FILES"


testHelpSwitch :: Test
testHelpSwitch = TestCase $ do
   (allOutput, _) <- Util.getBinaryOutput ["-h"]
   assertEqual "-h switch output"
      expectedFirstLine
      $ head $ lines allOutput


testHelpSwitchLong :: Test
testHelpSwitchLong = TestCase $ do
   (allOutput, _) <- Util.getBinaryOutput ["--help"]
   assertEqual "--help switch output"
      expectedFirstLine
      $ head $ lines allOutput
