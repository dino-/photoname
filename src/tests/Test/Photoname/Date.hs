module Test.Photoname.Date (
   tests
)
   where

import Data.Time.Calendar
import Data.Time.LocalTime
import Photoname.Date ( PhDate (..), parseExifDate, parseSignalDate )
import Test.HUnit


tests :: Test
tests = TestList
   [ TestLabel "testParseExifDate" testParseExifDate
   , TestLabel "testParseSignalDate" testParseSignalDate
   , TestLabel "testParseSignalDateMoreHyphens" testParseSignalDateMoreHyphens
   ]


expectedLocalTime :: LocalTime
expectedLocalTime = LocalTime (fromGregorian 2021 10 04)
  (TimeOfDay 17 29 (fromIntegral (49 :: Integer)))


testParseExifDate :: Test
testParseExifDate = ExifDate expectedLocalTime ~=?
  (parseExifDate $ Just "2021:10:04 17:29:49")


testParseSignalDate :: Test
testParseSignalDate = FilenameDate expectedLocalTime ~=?
  parseSignalDate "some/directory/signal-2021-10-04-172949.jpg"


testParseSignalDateMoreHyphens :: Test
testParseSignalDateMoreHyphens = FilenameDate expectedLocalTime ~=?
  parseSignalDate "some/directory/signal-2021-10-04-17-29-49-942.jpg"
