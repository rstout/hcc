module Main where

import Test.HUnit
import Compile.Frontend.Tests

main :: IO ()
main = do c <- runTestTT tests; return ()