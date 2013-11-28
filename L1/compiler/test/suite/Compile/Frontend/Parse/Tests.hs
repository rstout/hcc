module Compile.Frontend.Parse.Tests (tests) where

import Test.HUnit
import Control.Monad
import Data.ByteString.Char8 (pack)

import Compile.Frontend.Parse
import Compile.Frontend.TestUtils
import Compile.Types

tests :: Test
tests = TestLabel "Parser" $ TestList
        [ testSimpleProgramParsesWithSuccess
        , testValidDecimalsParseWithSuccess
        , testInvalidDecimalsParseWithFailure
        , testValidHexParsesWithSuccess
        , testInvalidHexParsesWithFailure
        , testUseOfReservedKeywordParsesWithFailure
        , testValidIdentifiersParseWithSuccess
        , testInvalidIdentifiersParseWithFailure
        , testValidAsnOpsParseWithSuccess
        , testInvalidAsnOpsParseWithFailure
        , testValidUnOpsParseWithSuccess
        , testInvalidUnOpsParseWithFailure
        , testValidBinOpsParseWithSuccess
        , testInvalidBinOpsParseWithFailure
        , testInvalidTypesParseWithFailure
        , testValidAsgnsParseWithSuccess
        , testInvalidAsgnsParseWithFailure
        , testValidDeclAsgnMixParsesWithSuccess
        , testInvalidMainFuncsParseWithFailure
        ]

assertParseSuccess :: String -> Assertion
assertParseSuccess prgm =
    case parseAST (SourceCode "" (pack prgm)) of
      Left msg -> assertFailure $ "Expected parse success but got:\n" ++ msg
      Right _ -> return ()

assertParseFailure :: String -> Assertion
assertParseFailure prgm =
    case parseAST (SourceCode "" (pack prgm)) of
      Left msg -> return ()
      Right _ -> assertFailure "Expected parse failure but got parse success"

testSimpleProgramParsesWithSuccess :: Test
testSimpleProgramParsesWithSuccess = TestLabel "Valid simple program" $
    TestCase $ assertParseSuccess $ stmtToC0Main "return 5"

testValidDecimalsParseWithSuccess :: Test
testValidDecimalsParseWithSuccess = TestLabel "Valid decimals" $
    let decmls = ["0", "1", "100", "123456789", "9058371", "43"]
        prgms = map (stmtToC0Main . ("int i = " ++)) decmls
    in  TestCase $ mapM_ assertParseSuccess prgms

testInvalidDecimalsParseWithFailure :: Test
testInvalidDecimalsParseWithFailure = TestLabel "Invalid decimals" $
    let decmls = ["01", "001", "0430"]
        prgms = map (stmtToC0Main . ("int i = " ++)) decmls
    in  TestCase $ mapM_ assertParseFailure prgms

testValidHexParsesWithSuccess :: Test
testValidHexParsesWithSuccess = TestLabel "Valid hex" $
    let hex = ["0xf", "0XAD14", "0xabcdef12", "0xffe45"]
        prgms = map (stmtToC0Main . ("int i = " ++)) hex
    in  TestCase $ mapM_ assertParseSuccess prgms

testInvalidHexParsesWithFailure :: Test
testInvalidHexParsesWithFailure = TestLabel "Invalid hex" $
    let hex = ["0x", "0X", "0xG1"]
        prgms = map (stmtToC0Main . ("int i = " ++)) hex
    in  TestCase $ mapM_ assertParseFailure prgms

testUseOfReservedKeywordParsesWithFailure :: Test
testUseOfReservedKeywordParsesWithFailure = TestLabel "Reserved keywords" $
    TestCase $ assertParseFailure $ stmtToC0Main "int return = 5"

testValidIdentifiersParseWithSuccess :: Test
testValidIdentifiersParseWithSuccess = TestLabel "Valid identifiers" $
    let idents = ["a", "i", "_A1_", "abcXYZ123", "_", "_10", "AaAa0"]
        prgms = map (stmtToC0Main . ("int " ++) . (++ " = 5")) idents
    in  TestCase $ mapM_ assertParseSuccess prgms

testInvalidIdentifiersParseWithFailure :: Test
testInvalidIdentifiersParseWithFailure = TestLabel "Invalid identifiers" $
    let idents = ["1one", "", ".,?", "+"]
        prgms = map (stmtToC0Main . ("int " ++) . (++ " = 5")) idents
    in  TestCase $ mapM_ assertParseFailure prgms

testValidAsnOpsParseWithSuccess :: Test
testValidAsnOpsParseWithSuccess = TestLabel "Valid AsnOps" $
    let asnOps = ["=", "+=", "-=", "*=", "/=", "%="]
        prgms = map (stmtToC0Main . ("i " ++) . (++ " 5")) asnOps
    in  TestCase $ mapM_ assertParseSuccess prgms

testInvalidAsnOpsParseWithFailure :: Test
testInvalidAsnOpsParseWithFailure = TestLabel "Invalid AsnOps" $
    let asnOps = ["wut", "8", "!", "!=", ".=", ",="]
        prgms = map (stmtToC0Main . ("i " ++) . (++ " 5")) asnOps
    in  TestCase $ mapM_ assertParseFailure prgms

testValidUnOpsParseWithSuccess :: Test
testValidUnOpsParseWithSuccess = TestLabel "Valid UnOps" $
    let unOps = ["-"]
        prgms = map (stmtToC0Main . ("int i = " ++) . (++ "1")) unOps
    in  TestCase $ mapM_ assertParseSuccess prgms

testInvalidUnOpsParseWithFailure :: Test
testInvalidUnOpsParseWithFailure = TestLabel "Invalid UnOps" $
    let unOps = ["&", "@", "$", "<"]
        prgms = map (stmtToC0Main . ("int i = " ++) . (++ "1")) unOps
    in  TestCase $ mapM_ assertParseFailure prgms

testValidBinOpsParseWithSuccess :: Test
testValidBinOpsParseWithSuccess = TestLabel "Valid BinOps" $
    let binOps = ["+", "-", "*", "/", "%"]
        prgms = map (stmtToC0Main . ("i = 1 " ++) . (++ " 1")) binOps
    in  TestCase $ mapM_ assertParseSuccess prgms

testInvalidBinOpsParseWithFailure :: Test
testInvalidBinOpsParseWithFailure = TestLabel "Invalid BinOps" $
    let binOps = ["&", "@", "$", "w", "1", "=", "+="]
        prgms = map (stmtToC0Main . ("i = 1 " ++) . (++ " 1")) binOps
    in  TestCase $ mapM_ assertParseFailure prgms

testInvalidTypesParseWithFailure :: Test
testInvalidTypesParseWithFailure = TestLabel "Invalid types" $
    let types = ["notAnInt"]
        prgms = map (stmtToC0Main . (++ " i = 1")) types
    in  TestCase $ mapM_ assertParseFailure prgms

testValidAsgnsParseWithSuccess :: Test
testValidAsgnsParseWithSuccess = TestLabel "Valid Asgns" $
    let stmts = ["i += 10", "(p) = 6", "((n)) -= 12"]
        prgms = map stmtToC0Main stmts
    in  TestCase $ mapM_ assertParseSuccess prgms

testInvalidAsgnsParseWithFailure :: Test
testInvalidAsgnsParseWithFailure = TestLabel "Invalid Asgns" $
    let stmts = ["i = = 20", "j +=* 2", "k += -= 1", "(l = 4",
                 ")u = 9", "o) = 8"]
        prgms = map stmtToC0Main stmts
    in  TestCase $ mapM_ assertParseFailure prgms

testValidDeclAsgnMixParsesWithSuccess :: Test
testValidDeclAsgnMixParsesWithSuccess = TestLabel "Valid Decl Asgn mix" $
    let stmts = ["int a = 1", "a += 2", "return a"]
        prgm = stmtsToC0Main stmts
    in  TestCase $ assertParseSuccess prgm

testInvalidMainFuncsParseWithFailure :: Test
testInvalidMainFuncsParseWithFailure = TestLabel "Invalid main" $
    let prgms = [ "int f () { return 2; }", "int main () { return 1;"
                , "int main () return 1;", "int main { return 1; }"
                , "main () { return 0; }", "int main() {}"
                ]
    in  TestCase $ mapM_ assertParseFailure prgms
