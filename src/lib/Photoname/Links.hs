module Photoname.Links
  ( describeHardLinkPolicy
  , linksTest
  )
  where

import System.Posix ( FileStatus, linkCount )
import Text.Printf ( printf )

import Photoname.Common ( Links (Exactly, NoLimit) )
import Photoname.Log ( infoM, lname )


linksTest :: Links -> FileStatus -> Bool
linksTest (Exactly linkCountWanted) fileStatus = linkCountWanted == linkCount fileStatus
linksTest NoLimit                   _          = True


describeHardLinkPolicy :: Links -> IO ()
describeHardLinkPolicy l = case l of
  Exactly 1 -> infoM lname $ printf "Only processing files with 1 hard link"
  Exactly n -> infoM lname $ printf "Only processing files with %s hard links" (show n)
  NoLimit   -> pure ()

