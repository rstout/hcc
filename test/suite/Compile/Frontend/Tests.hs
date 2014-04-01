module Hcc.Compile.Frontend.Tests (tests) where

import Test.HUnit
import qualified Compile.Frontend.Parse.Tests
import qualified Compile.Frontend.CheckAST.Tests

tests :: Test
tests = TestLabel "Frontend" $ TestList
        [ Compile.Frontend.Parse.Tests.tests
        , Compile.Frontend.CheckAST.Tests.tests
        ]
