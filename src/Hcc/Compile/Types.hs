-- | Types.hs

module Hcc.Compile.Types
       ( module X
       , SourceCode(..)
       , TargetCode
--       , AST(..)
       , IR(..)
       ) where

import Hcc.Compile.Types.Job as X
import Hcc.Compile.Types.AbstractAssembly as X
import Hcc.Compile.Types.Ops as X
import Hcc.Compile.Types.AST as X
import Hcc.Compile.Types.IR as X

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
