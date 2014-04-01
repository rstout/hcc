module Main where

import Test.HUnit
import qualified Hcc.Compile.Tests

main :: IO ()
main = do runTestTT $ TestList tests
          return ()

tests :: [Test]
tests = concat
        [ Hcc.Compile.Tests.tests
        ]