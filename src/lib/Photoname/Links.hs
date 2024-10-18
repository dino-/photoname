{-# LANGUAGE OverloadedStrings #-}

module Photoname.Links
  ( describeHardLinkPolicy
  , linksTest
  )
  where

import Formatting ((%+), formatToString, int)
import System.Posix (FileStatus, linkCount)

import Photoname.Common (Links (Exactly, NoLimit))
import Photoname.Log (infoM, lname)


linksTest :: Links -> FileStatus -> Bool
linksTest (Exactly linkCountWanted) fileStatus = linkCountWanted == linkCount fileStatus
linksTest NoLimit                   _          = True


describeHardLinkPolicy :: Links -> IO ()
describeHardLinkPolicy l = case l of
  Exactly 1 -> infoM lname          "Only processing files with 1 hard link"
  Exactly n -> infoM lname
    $ formatToString ("Only processing files with" %+ int %+ "hard links") (toInteger n)
  NoLimit   -> pure ()

