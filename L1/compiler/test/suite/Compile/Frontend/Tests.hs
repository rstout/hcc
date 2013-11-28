module Compile.Frontend.Tests (tests) where

import Test.HUnit
import qualified Compile.Frontend.Parse.Tests

tests :: Test
tests = TestLabel "Frontend tests" $ TestList
        [ Compile.Frontend.Parse.Tests.tests
        ]
