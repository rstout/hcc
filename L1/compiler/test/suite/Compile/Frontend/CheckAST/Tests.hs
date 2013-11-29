module Compile.Frontend.CheckAST.Tests (tests) where

import Test.HUnit
import Control.Monad
import Data.ByteString.Char8 (pack)

import Compile.Frontend.Parse
import Compile.Frontend.CheckAST
import Compile.Frontend.TestUtils
import Compile.Types

tests :: Test
tests = TestLabel "CheckAST" $ TestList
        [ testMissingReturnChecksWithFailure
        , testReturnUndeclVarChecksWithFailure
        , testMulitDeclsChecksWithFailure
        , testUseBeforeDeclChecksWithFailure
        , testUseBeforeInitChecksWithFailure
        , testInvalidDecimalChecksWithFailure
        , testInvalidHexChecksWithFailure
        ]

assertCheckFailure :: String -> Assertion
assertCheckFailure prgm =
    case parseAST (SourceCode "" (pack prgm)) of
      Left _ -> assertFailure "Expected checkAST failure but got parse failure"
      Right ast ->
          case checkAST ast of
            Left _ -> return ()
            Right _ -> assertFailure
                       "Expected checkAST failure but got checkAST success"

assertCheckFailureStmt :: String -> Assertion
assertCheckFailureStmt stmt = assertCheckFailure $ stmtToC0Main stmt

assertCheckFailureStmts :: [String] -> Assertion
assertCheckFailureStmts stmts = assertCheckFailure $ stmtsToC0Main stmts

testMissingReturnChecksWithFailure :: Test
testMissingReturnChecksWithFailure =
    TestLabel "Missing return" $
    TestCase $ assertCheckFailureStmt "int i = 1"

testReturnUndeclVarChecksWithFailure :: Test
testReturnUndeclVarChecksWithFailure =
    TestLabel "Return undecl" $
    TestCase $ assertCheckFailureStmt "return a"

testMulitDeclsChecksWithFailure :: Test
testMulitDeclsChecksWithFailure =
    TestLabel "Multiple decls" $
    TestCase $ assertCheckFailureStmts ["int i = 5", "int i = 6"]

testUseBeforeDeclChecksWithFailure :: Test
testUseBeforeDeclChecksWithFailure =
    TestLabel "Use before decl" $
    TestCase $ assertCheckFailureStmt "i = 5"

testUseBeforeInitChecksWithFailure :: Test
testUseBeforeInitChecksWithFailure =
    TestLabel "Use before init" $
    TestCase $ assertCheckFailureStmts ["int i", "int a = i + 1"]

testInvalidDecimalChecksWithFailure :: Test
testInvalidDecimalChecksWithFailure =
    TestLabel "Invalid decimal" $
    TestCase $ assertCheckFailureStmt "int i = 2147483649"

testInvalidHexChecksWithFailure :: Test
testInvalidHexChecksWithFailure =
    TestLabel "Invalid hex" $
    TestCase $ assertCheckFailureStmt "int i = 0xaabbccddff"
