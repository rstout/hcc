{-| SimpleStmt.hs

    Converts a valid AST to a list of simple statements
-}

module Hcc.Compile.IR.SimpleStmt (genSimpleStmts) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Int
import Data.Functor ((<$>))
import Control.Monad.State
import Control.Monad

--import Hcc.Compile.Types.IR (SimpleStmt(..), FirstClassValue(..))
import Hcc.Compile.Types

genSimpleStmts :: AST -> [SimpleStmt]
genSimpleStmts ast = evalState (astToSimpleStmt ast) newVarState

astToSimpleStmt :: AST -> State VarState [SimpleStmt]
astToSimpleStmt (Block stmts _) = concat <$> mapM stmtToSimpleStmt stmts

stmtToSimpleStmt :: Stmt -> State VarState [SimpleStmt]
stmtToSimpleStmt (Decl ident mExpr _) =
    do { destTemp <- assignTemp ident
       ; case mExpr of
           Nothing -> return []
           Just expr -> do { (aasm, srcTemp) <- exprToSimpleStmt expr
                           ; return $ aasm ++ [Assign destTemp (Var srcTemp)]
                           }
       }

stmtToSimpleStmt (Asgn ident asgnOp expr _) =
    do { destTemp <- getTemp ident
       ; (aasm, srcTemp) <- exprToSimpleStmt expr
       ; return $ aasm ++ case asgnOp of
                            Nothing -> [Assign destTemp (Var srcTemp)]
                            Just op -> [AssignBinary destTemp (Var destTemp) op (Var srcTemp)]
       }

stmtToSimpleStmt (Return expr _) =
    do { (aasm, srcTemp) <- exprToSimpleStmt expr
       ; return $ aasm ++ [Ret $ Var srcTemp]
       }

exprToSimpleStmt :: Expr -> State VarState ([SimpleStmt], Var)
exprToSimpleStmt (ExprInt i _) =
    do { temp <- newTemp
       ; return ([Assign temp (IntConst $ toInt32 i)], temp)
       }

exprToSimpleStmt (Ident ident _) = (,) [] <$> getTemp ident

exprToSimpleStmt (ExprUnOp op expr _) =
    do { (aasm, srcTemp) <- exprToSimpleStmt expr
       ; destTemp <- newTemp
       ; return (aasm ++ [AssignUnary destTemp op (Var srcTemp)], destTemp)
       }

exprToSimpleStmt (ExprBinOp op e1 e2 _) =
    do { (aasm1, srcTemp1) <- exprToSimpleStmt e1
       ; (aasm2, srcTemp2) <- exprToSimpleStmt e2
       ; destTemp <- newTemp
       ; let aasm = aasm1 ++ aasm2 ++ [AssignBinary destTemp (Var srcTemp1) op (Var srcTemp2)]
       ; return (aasm, destTemp)
       }

data VarState = VarState { varMap :: Map.Map String Var
                         , nextVar :: Var
                         }

newVarState = VarState { varMap = Map.empty, nextVar = 0 }

newTemp :: State VarState Var
newTemp = do { temp <- gets nextVar
             ; modify $ \ts -> ts { nextVar = temp + 1 }
             ; return temp
             }

-- Assigns a Temp to the given identifier
assignTemp :: String -> State VarState Var
assignTemp ident =
    do { temp <- newTemp
       ; modify $ \ts -> ts { varMap = Map.insert ident temp (varMap ts) }
       ; return temp
       }

getTemp :: String -> State VarState Var
getTemp ident = do { temps <- gets varMap
                   ; return $ temps Map.! ident
                   }

toInt32 :: Integer -> Int32
toInt32 = fromIntegral
