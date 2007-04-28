#!/usr/bin/env runhaskell

> import Distribution.Simple
> import System.Cmd


> main = defaultMainWithHooks (defaultUserHooks { runTests = testRunner } )
>     where
>        testRunner _ _ _ _ =
>           system $ "runhaskell -itestsuite testsuite/runtests.hs"
