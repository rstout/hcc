{- AAsm.hs

   Defines the Abstract Assembly types
-}

module Compile.Types.AAsm where

import Data.Int (Int32)

import Compile.Types.Ops

-- A temporary/variable
type Temp = Integer

data Primitive = Var Temp | Const Int32
                 deriving Show

-- 3 operand Abstract Assembly, of the form:
-- operand1 = operand2 operator operand3
data AAsm3Op = AsgnPrim       Temp Primitive
             | AsgnPrimUnary  Temp Op Primitive
             | AsgnPrimBinary Temp Primitive Op Primitive
             | Ret Primitive
               deriving Show