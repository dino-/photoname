#!/usr/bin/env runhaskell

> import Distribution.Simple
> import System.Cmd


> main = defaultMainWithHooks (simpleUserHooks { runTests = testRunner } )
>     where
>        testRunner _ _ _ _ = do
>           system $ "runhaskell -itestsuite testsuite/runtests.hs"
>           return ()
