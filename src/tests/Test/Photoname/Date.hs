{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Photoname.Date
  ( tests
  )
  where

import Data.Time.Calendar
import Data.Time.LocalTime
import Photoname.Common ( SrcPath (..) )
import Photoname.Date ( PhDate (..), parseExifDate, parseFilenameDate )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


expectedLocalTime :: LocalTime
expectedLocalTime = LocalTime (fromGregorian 2021 10 04)
  (TimeOfDay 17 29 (fromIntegral (49 :: Integer)))


tests :: TestTree
tests = testGroup "Photoname.Date"
  [ parsingTests
  , propsPhDate
  ]


parsingTests :: TestTree
parsingTests = testGroup "parsing tests"
  [ testCase "parse a datetime in EXIF format" $
      ExifDate expectedLocalTime @=?
        (parseExifDate $ Just "2021:10:04 17:29:49")
  , testCase "parse a date from a file path with directories" $
      FilenameDate expectedLocalTime @=?
        parseFilenameDate (SrcPath "some/directory/signal-2021-10-04-172949.jpg")
  , testCase "parse a date from a file path with directories and more hyphens" $
      FilenameDate expectedLocalTime @=?
        parseFilenameDate (SrcPath "some/directory/signal-2021-10-04-17-29-49-942.jpg")
  , testCase "parse a date from a file path with no directories" $
      FilenameDate expectedLocalTime @=?
        parseFilenameDate (SrcPath "signal-2021-10-04-172949.jpg")
  , testCase "parse a date from a file path with more hyphens but no directories" $
      FilenameDate expectedLocalTime @=?
        parseFilenameDate (SrcPath "signal-2021-10-04-17-29-49-942.jpg")
  , testCase "parse a date from a previously-named file path" $
      FilenameDate expectedLocalTime @=?
        parseFilenameDate (SrcPath "20211004-172949.jpg")
  , testCase "parse a date from a previously-named file path with suffix" $
      FilenameDate expectedLocalTime @=?
        parseFilenameDate (SrcPath "20211004-172949_xyz.jpg")
  , testCase "parse a date from a Pixel phone path file path" $
      FilenameDate expectedLocalTime @=?
        parseFilenameDate (SrcPath "PXL_20211004_172949000.jpg")
  , testCase "parse a date from a Pixel phone path file path with extra suffix" $
      FilenameDate expectedLocalTime @=?
        parseFilenameDate (SrcPath "PXL_20211004_172949000_foo.jpg")
  ]


instance Arbitrary PhDate where
  arbitrary = do
    oneof
      [ return $ ExifDate expectedLocalTime
      , return $ FilenameDate expectedLocalTime
      , return NoDateFound
      ]

propsPhDate :: TestTree
propsPhDate = testGroup "testing the Semigroup and Monoid properties of PhDate"
  [ testProperty "Semigroup associativity  x <> (y <> z) == (x <> y) <> z" $
    \(x :: PhDate) (y :: PhDate) (z :: PhDate) ->
      x <> (y <> z) == (x <> y) <> z
  , testProperty "Monoid right identity  x <> mempty == x" $
    \(x :: PhDate) -> x <> mempty == x
  , testProperty "Monoid left identity  mempty <> x == x" $
    \(x :: PhDate) -> mempty <> x == x
  , testProperty "Monoid concatenation  mconcat xs == foldr (<>) mempty xs" $
    \(xs :: [PhDate]) -> mconcat xs == foldr (<>) mempty xs
  ]
