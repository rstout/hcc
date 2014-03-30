{- L1 Compiler
   Author: Ryan Stout rstout610@yahoo.com

   Beginnings of a typechecker
-}

module Compile.Frontend.CheckAST (checkAST) where

import Control.Monad.State
import Control.Monad
import Data.Functor
import Text.ParserCombinators.Parsec.Pos (SourcePos)
import qualified Data.Map as Map
import Data.List

import Compile.Types

checkAST :: AST -> Either String AST
checkAST ast = case errors $ execState (check ast) newCheckState of
                 [] -> Right ast
                 xs -> Left $ intercalate "\n" xs

class Check a where
    check :: a -> State CheckState ()

instance Check AST where
    check (Block stmts _) = do mapM check stmts; checkReturnExists

instance Check Stmt where
    check (Decl ident e pos) =
        do { checkMultiDecl ident pos
           ; case e of Just expr -> do check expr; addInitDecl ident
                       Nothing   -> do addUninitDecl ident
           }

    check (Asgn ident op e pos) =
        do { check e
           ; checkIsDecled ident pos
           ; case op of
               Just _ -> checkIsInited ident pos
               Nothing -> return ()
           ; addInitDecl ident
           }

    check (Return e _) = do check e; foundReturn

instance Check Expr where
    check (ExprInt i pos) =
        do { if i > int32Max
             then addError $ "Integer to large at: " ++ show pos
             else return ()
           }

    check (Ident ident pos) = do checkIsInited ident pos
    check (ExprBinOp op e1 e2 _) = do check e1; check e2
    check (ExprUnOp op e _) = do check e


-- Defines the state of the AST checker
data CheckState = CheckState { declMap :: Map.Map String Bool
                             , errors :: [String]
                             , returnExists :: Bool
                             }

newCheckState :: CheckState
newCheckState = CheckState { declMap = Map.empty
                           , errors = []
                           , returnExists = False
                           }

addError :: String -> State CheckState ()
addError msg = do modify $ \cs -> cs { errors = msg : (errors cs) }

foundReturn :: State CheckState ()
foundReturn = do modify $ \cs -> cs { returnExists = True }

-- Add an initialized declaration to the CheckState
addInitDecl :: String -> State CheckState ()
addInitDecl decl = do
  modify $ \cs -> cs { declMap = Map.insert decl True (declMap cs) }

-- Add an uninitialized declaration to the CheckState
addUninitDecl :: String -> State CheckState ()
addUninitDecl decl = do
  modify $ \cs -> cs { declMap = Map.insert decl False (declMap cs) }

checkMultiDecl :: String -> SourcePos -> State CheckState ()
checkMultiDecl ident pos =
    do { cs <- get
       ; case Map.lookup ident $ declMap cs of
           Just _ -> do addError $ "Mutliple declarations of \"" ++ ident ++
                                    "\" at: " ++ show pos
           Nothing -> return ()
       }

checkReturnExists :: State CheckState ()
checkReturnExists = do { cs <- get
                       ; if (not $ returnExists cs)
                         then addError "Return statment is missing"
                         else return ()
                       }

checkIsDecled :: String -> SourcePos -> State CheckState ()
checkIsDecled ident pos =
    do { cs <- get
       ; case Map.lookup ident (declMap cs) of
           Just _ -> return ()
           Nothing -> addError $ "Variable \"" ++ ident ++
                      "\" used before being declared at: " ++ show pos
       }

checkIsInited :: String -> SourcePos -> State CheckState ()
checkIsInited ident pos =
    do { cs <- get
       ; case Map.lookup ident (declMap cs) of
           Just True -> return ()
           _ -> addError $ "Variable \"" ++ ident ++
                "\" used before being initialized at: " ++ show pos
       }

int32Max :: Integer
int32Max = 2 ^ 31
