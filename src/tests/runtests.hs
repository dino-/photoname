module Main
   where

import System.Environment ( getArgs, withArgs )
import Test.Tasty

import qualified TestLink ( tests )
import qualified Test.Photoname.Date ( tests )


{- Defaults to only running the unit tests. To run everything:

    $ stack test --ta all
    $ stack test --test-arguments all
-}

main :: IO ()
main = do
  args <- getArgs
  let baseTests = [ Test.Photoname.Date.tests ]
  let tests = if "all" `elem` args
        then TestLink.tests : baseTests
        else baseTests
  withArgs [] $ defaultMain $ testGroup " tests" tests
