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
   along with Foobar; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  
   02110-1301  USA
-}

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
