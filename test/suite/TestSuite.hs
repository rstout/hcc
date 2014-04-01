module Main where

import Test.HUnit
import Hcc.Compile.Tests

main :: IO ()
main = do runTestTT tests; return ()
