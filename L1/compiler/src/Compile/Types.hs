{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>
-}
module Compile.Types 
       ( module X
       , SourceCode(..)
       , TargetCode
--       , AST(..)
       , IR(..)
       ) where

import Compile.Types.Job as X
import Compile.Types.AbstractAssembly as X
import Compile.Types.Ops as X
import Compile.Types.AST as X

import Data.ByteString (ByteString)

data SourceCode = SourceCode FilePath ByteString
type TargetCode = String

data IR = IR String -- placeholder

{-
class AST a where
  genIR :: (IR b) => a -> b
           
class IR a where
  genTargetCode :: a -> TargetCode
-} 