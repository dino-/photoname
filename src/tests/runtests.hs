module Main
   where

import Test.Tasty

import qualified TestLink ( tests )
import qualified Test.Photoname.Date ( tests )


main :: IO ()
main = defaultMain $ testGroup " tests"
  [ Test.Photoname.Date.tests
  , TestLink.tests
  ]
