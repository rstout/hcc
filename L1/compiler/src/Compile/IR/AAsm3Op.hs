{-| AAsm3Op.hs

    Converts a valid AST to AAsm3Op form
-}

module Compile.IR.AAsm3Op (genAAsm3Op) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Int
import Data.Functor ((<$>))
import Control.Monad.State
import Control.Monad

import Compile.Types

genAAsm3Op :: AST -> [AAsm3Op]
genAAsm3Op ast = evalState (astToAAsm3Op ast) newTempState

astToAAsm3Op :: AST -> State TempState [AAsm3Op]
astToAAsm3Op (Block stmts _) = concat <$> mapM stmtToAAsm3Op stmts

stmtToAAsm3Op :: Stmt -> State TempState [AAsm3Op]
stmtToAAsm3Op (Decl ident mExpr _) =
    do { destTemp <- assignTemp ident
       ; case mExpr of
           Nothing -> return []
           Just expr -> do { (aasm, srcTemp) <- exprToAAsm3Op expr
                           ; return $ aasm ++ [AsgnPrim destTemp (Var srcTemp)]
                           }
       }

stmtToAAsm3Op (Asgn ident asgnOp expr _) =
    do { destTemp <- getTemp ident
       ; (aasm, srcTemp) <- exprToAAsm3Op expr
       ; return $ aasm ++ case asgnOp of
                            Nothing -> [AsgnPrim destTemp (Var srcTemp)]
                            Just op -> [AsgnPrimBinary destTemp (Var destTemp) op (Var srcTemp)]
       }

stmtToAAsm3Op (Return expr _) =
    do { (aasm, srcTemp) <- exprToAAsm3Op expr
       ; return $ aasm ++ [Ret $ Var srcTemp]
       }

exprToAAsm3Op :: Expr -> State TempState ([AAsm3Op], Temp)
exprToAAsm3Op (ExprInt i _) =
    do { temp <- newTemp
       ; return ([AsgnPrim temp (Const $ toInt32 i)], temp)
       }

exprToAAsm3Op (Ident ident _) = (,) [] <$> getTemp ident

exprToAAsm3Op (ExprUnOp op expr _) =
    do { (aasm, srcTemp) <- exprToAAsm3Op expr
       ; destTemp <- newTemp
       ; return (aasm ++ [AsgnPrimUnary destTemp op (Var srcTemp)], destTemp)
       }

exprToAAsm3Op (ExprBinOp op e1 e2 _) =
    do { (aasm1, srcTemp1) <- exprToAAsm3Op e1
       ; (aasm2, srcTemp2) <- exprToAAsm3Op e2
       ; destTemp <- newTemp
       ; let aasm = aasm1 ++ aasm2 ++ [AsgnPrimBinary destTemp (Var srcTemp1) op (Var srcTemp2)]
       ; return (aasm, destTemp)
       }

data TempState = TempState { tempMap :: Map.Map String Temp
                           , nextTemp :: Temp
                           }

newTempState = TempState { tempMap = Map.empty, nextTemp = 0 }

newTemp :: State TempState Temp
newTemp = do { temp <- gets nextTemp
             ; modify $ \ts -> ts { nextTemp = temp + 1 }
             ; return temp
             }

-- Assigns a Temp to the given identifier
assignTemp :: String -> State TempState Temp
assignTemp ident =
    do { temp <- newTemp
       ; modify $ \ts -> ts { tempMap = Map.insert ident temp (tempMap ts) }
       ; return temp
       }

getTemp :: String -> State TempState Temp
getTemp ident = do { temps <- gets tempMap
                   ; return $ temps Map.! ident
                   }

toInt32 :: Integer -> Int32
toInt32 = fromIntegral
