{-
   Copyright 2007 Dino Morelli

   This file is part of photoname

   photoname is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   photoname is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with photoname; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  
   02110-1301  USA
-}

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
