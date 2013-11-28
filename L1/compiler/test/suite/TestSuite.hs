module Main where

import Test.HUnit
import Compile.Tests

main :: IO ()
main = do runTestTT tests; return ()