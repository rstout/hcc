module Compile.Frontend.TestUtils where

import Data.List

stmtToC0Main :: String -> String
stmtToC0Main stmt = "int main() { " ++ stmt ++ "; }"

stmtsToC0Main :: [String] -> String
stmtsToC0Main stmts = "int main() { " ++ (intercalate "; " stmts) ++ "; }"