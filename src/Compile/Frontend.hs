{-| Frontend.hs 
    Author: Ryan Stout 

    File parsing, AST construction, AST validation, Type checking, etc.
-} 

module Compile.Frontend where

import Compile.Frontend.CheckAST
import Compile.Frontend.Parse
import Compile.Types

import Data.Functor
  
genAST :: SourceCode -> Either String AST
genAST source = checkAST =<< parseAST source
