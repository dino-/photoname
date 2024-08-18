module Main
   where

import System.Environment ( getArgs, withArgs )
import Test.Tasty

import qualified Photoname.Test.EndToEnd.Link as Link
import qualified Photoname.Test.Unit.Date as Date


{- Defaults to only running the unit tests. To run everything:

    $ stack test --ta all
    $ stack test --test-arguments all
-}

main :: IO ()
main = do
  args <- getArgs
  let baseTests = [ Date.tests ]
  let tests = if "all" `elem` args
        then Link.tests : baseTests
        else baseTests
  withArgs [] $ defaultMain $ testGroup " tests" tests
