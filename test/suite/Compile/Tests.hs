module Hcc.Compile.Tests (tests) where

import Test.HUnit
import qualified Compile.Frontend.Tests

tests :: Test
tests = TestList
        [ Compile.Frontend.Tests.tests
        ]
