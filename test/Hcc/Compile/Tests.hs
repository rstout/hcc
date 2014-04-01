module Hcc.Compile.Tests (tests) where

import Test.HUnit
import qualified Hcc.Compile.Frontend.Tests

tests :: [Test]
tests = concat
        [ Hcc.Compile.Frontend.Tests.tests
        ]
