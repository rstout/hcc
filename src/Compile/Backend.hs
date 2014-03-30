{-| Backend.hs 
    Author: Ryan Stout 

    Register allocation, instruction selection, target-code generation
-}

module Compile.Backend where

import Compile.Types

genTargetCode :: IR -> TargetCode
genTargetCode = undefined