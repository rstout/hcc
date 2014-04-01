{-| Frontend.hs
    Author: Ryan Stout

    File parsing, AST construction, AST validation, Type checking, etc.
-}

module Hcc.Compile.Frontend where

import Data.Functor

import Hcc.Compile.Frontend.CheckAST
import Hcc.Compile.Frontend.Parse
import Hcc.Compile.Types

genAST :: SourceCode -> Either String AST
genAST source = checkAST =<< parseAST source
