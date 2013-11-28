module Compile.Frontend.Parse.Tests (tests) where

import Test.HUnit
import Compile.Frontend.Parse
import Compile.Types
import Data.ByteString.Char8 (pack)

tests :: Test
tests = TestLabel "Parser tests" $ TestList
        [ testSimpleProgramParsesWithSuccess
        ]

assertParseSuccess :: String -> Assertion
assertParseSuccess prgm =
    case parseAST (SourceCode "" (pack prgm)) of
      Left msg -> assertFailure msg -- "Expected parse success but got: ..."
      Right _ -> return ()

testSimpleProgramParsesWithSuccess :: Test
testSimpleProgramParsesWithSuccess =
    TestCase (assertParseSuccess prgm) where
        prgm = "int main () { return 5; }"