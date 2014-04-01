module Hcc.Compile.Frontend.Tests (tests) where

import Test.HUnit (Test(..))
import qualified Hcc.Compile.Frontend.Parse.Tests
import qualified Hcc.Compile.Frontend.CheckAST.Tests

tests :: [Test]
tests = [ TestLabel "Hcc.Compile.Frontend.Parse.Tests" $
          TestList Hcc.Compile.Frontend.Parse.Tests.tests
        , TestLabel "Hcc.Compile.Frontend.CheckAST.Tests" $
          TestList Hcc.Compile.Frontend.CheckAST.Tests.tests
        ]
