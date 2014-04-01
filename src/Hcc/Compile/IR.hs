{-| IR.hs
    Author: Ryan Stout

    Generates intermediate representations
-}

module Hcc.Compile.IR where

import Hcc.Compile.Types
import Hcc.Compile.IR.CodeGen
import Hcc.Compile.IR.SimpleStmt

genIR :: AST -> [SimpleStmt]
genIR = genSimpleStmts
