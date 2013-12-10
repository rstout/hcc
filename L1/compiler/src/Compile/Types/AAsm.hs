{- AAsm.hs

   Defines the Abstract Assembly types
-}

module Compile.Types.AAsm where

import Data.Int (Int32)

import Compile.Types.Ops

type Temp = Integer

data Primitive = Var Temp | Const Int32
                 deriving Show

data AAsm3Op = AsgnPrim       Temp Primitive
             | AsgnPrimUnary  Temp Op Primitive
             | AsgnPrimBinary Temp Primitive Op Primitive
             | Ret Primitive
               deriving Show