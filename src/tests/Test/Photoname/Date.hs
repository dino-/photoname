module Test.Photoname.Date
  ( tests
  )
  where

import Data.Time.Calendar
import Data.Time.LocalTime
import Photoname.Date ( PhDate (..), parseExifDate, parseSignalDate )
import Test.Tasty
import Test.Tasty.HUnit


expectedLocalTime :: LocalTime
expectedLocalTime = LocalTime (fromGregorian 2021 10 04)
  (TimeOfDay 17 29 (fromIntegral (49 :: Integer)))


tests :: TestTree
tests = testGroup "Photoname.Date"
  [ testCase "parse a datetime in EXIF format" $
      ExifDate expectedLocalTime @=?
        (parseExifDate $ Just "2021:10:04 17:29:49")
  , testCase "parse a date from a signal jpg filename" $
      FilenameDate expectedLocalTime @=?
        parseSignalDate "some/directory/signal-2021-10-04-172949.jpg"
  , testCase "parse a date from a signal jpg filename with more hyphens" $
      FilenameDate expectedLocalTime @=?
        parseSignalDate "some/directory/signal-2021-10-04-17-29-49-942.jpg"
  ]
