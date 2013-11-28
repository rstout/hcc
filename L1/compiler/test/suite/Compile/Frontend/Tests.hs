module Compile.Frontend.Tests (tests) where

import Test.HUnit.Base
import qualified Compile.Frontend.ParseTests

tests :: Test
tests = TestLabel "Frontend" $ TestList
        [ Compile.Frontend.ParseTests.tests
        ]
