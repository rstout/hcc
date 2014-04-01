{- IR.hs

   Defines types used to represent and process this compiler's IRs
   (Intermediate Representations)
   Use: The AST is converted to [SimpleStmt] to simplify further processing.
-}

module Hcc.Compile.Types.IR where

import Data.Int (Int32)

import Hcc.Compile.Types.Ops

-- | Internally, variables are represented by unique integer IDs
type Var = Integer

-- | Defines our language's first class values. A first class value is a
-- value that can be assigned to variables, passed into functions, and returned
-- by functions.
data FirstClassValue = Var Var        -- Variables
                     | IntConst Int32 -- Integer constants
                       deriving Show

-- | Simple Statment type
-- Use: The AST is converted to [SimpleStmt] to simplify further processing
-- Simple statements have one of the following forms:
-- 1. var = firstClassValue
--    -> e.g. x = 12
-- 2. var = unaryOperator firstClassValue
--    -> e.g. x = -y
-- 3. var = firstClassValue1 binaryOperator firstClassValue2
--    -> e.g. x = y + 2
-- 4. return firstClassValue
--    -> e.g. return x
data SimpleStmt = Assign       Var FirstClassValue
                | AssignUnary  Var Op FirstClassValue
                | AssignBinary Var FirstClassValue Op FirstClassValue
                | Ret FirstClassValue
                  deriving Show
