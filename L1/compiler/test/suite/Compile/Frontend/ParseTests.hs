module Compile.Frontend.ParseTests (tests) where

import Test.HUnit.Base hiding  (path)
import Compile.Frontend.Parse
import Compile.Types
import Data.ByteString.Char8 (pack)

tests :: Test
tests = TestLabel "Parse tests" $ TestList
        [ testSimpleProgramParsesWithSuccess
        ]

assertParseSuccess :: String -> Assertion
assertParseSuccess prgm =
    case parseAST (SourceCode "" (pack prgm)) of
      Left msg -> assertFailure msg
      Right _ -> return ()

testSimpleProgramParsesWithSuccess :: Test
testSimpleProgramParsesWithSuccess =
    TestCase (assertParseSuccess prgm) where
        prgm = "int main () { return 5; }"