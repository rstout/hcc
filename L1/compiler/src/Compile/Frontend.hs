{-| Frontend.hs 
    Author: Ryan Stout 

    File parsing, AST construction, AST validation, Type checking, etc.
-} 

module Compile.Frontend 
       ( parseAST
       , checkAST
       ) where

import Compile.Frontend.CheckAST
import Compile.Frontend.Parse
