module Processor.EvaluateStmt where

import Control.Monad.Except
import Grammar.Abs
import Grammar.ErrM
import Processor.Evaluate

--                                             | VBool {bool :: Bool}
--                                             | VNum {num :: Integer}
--                                             | VStr {str :: String}
--                                             | VFun {fun :: TopDef, env :: Env}
--                                             | VVoid
--                                             | VBreak
--                                             | VCont
--    | EVar Ident
--    | ELitInt Integer
--    | ELitTrue
--    | ELitFalse
--    | EApp Ident [Expr]
--    | EString String
--    | ELambda [Arg] Type Block
--    | Neg Expr
--    | Not Expr
--    | EMul Expr MulOp Expr
--    | EAdd Expr AddOp Expr
--    | ERel Expr RelOp Expr
--    | EAnd Expr Expr
--    | EOr Expr Expr

-- TODO: Relational operations should be evaluated here
evalAddOp :: AddOp -> Value -> Value -> Result Value
evalAddOp Plus (VNum v1) (VNum v2) = return $ VNum $ v1 + v2
evalAddOp Plus (VStr v1) (VStr v2) = return $ VStr $ v1 ++ v2
evalAddOp Plus v1 v2 = throwError $ "Cannot add values of type " ++ show v1 ++ " and " ++ show v2
evalAddOp Minus (VNum v1) (VNum v2) = return $ VNum $ v1 - v2
evalAddOp Minus v1 v2 = throwError $ "Cannot subtract values of type " ++ show v1 ++ " and " ++ show v2

evalMulOp :: MulOp -> Value -> Value -> Result Value
evalMulOp Times (VNum v1) (VNum v2) = return $ VNum $ v1 * v2
evalMulOp Times v1 v2 = throwError $ "Cannot multiply values of type " ++ show v1 ++ " and " ++ show v2
evalMulOp Div (VNum v1) (VNum v2)
  | v2 == 0 = throwError "Cannot divide by 0"
  | otherwise = return $ VNum $ div v1 v2
evalMulOp Div v1 v2 = throwError $ "Cannot divide values of type " ++ show v1 ++ " and " ++ show v2
evalMulOp Mod (VNum v1) (VNum v2)
  | v2 == 0 = throwError "Cannot find remainder for division by 0"
  | otherwise = return $ VNum $ mod v1 v2

isLess :: Value -> Value -> Result Value
isLess (VBool v1) (VBool v2) = return . VBool $ (v1 < v2)
isLess (VNum v1) (VNum v2) = return . VBool $ (v1 < v2)
isLess (VStr v1) (VStr v2) = return . VBool $ (v1 < v2)
isLess v1 v2 = throwError $ "Cannot comapre values of type " ++ show v1 ++ " and " ++ show v2

isEqual :: Value -> Value -> Result Value
isEqual v1 v2 = do
  (VBool res1) <- isLess v1 v2
  (VBool res2) <- isLess v2 v1
  return . VBool $ not res1 && not res2

-- LTH | LE | GTH | GE | EQU | NE
evalRelOp :: RelOp -> Value -> Value -> Result Value
evalRelOp LTH v1 v2 = isLess v1 v2
evalRelOp LE v1 v2 = do
  (VBool res1) <- isLess v1 v2
  (VBool res2) <- isEqual v1 v2
  return . VBool $ res1 || res2
evalRelOp GTH v1 v2 = isLess v2 v1
evalRelOp GE v1 v2 = do
  (VBool res1) <- isLess v2 v1
  (VBool res2) <- isEqual v1 v2
  return . VBool $ res1 || res2
evalRelOp EQU v1 v2 = isEqual v1 v2
evalRelOp NE v1 v2 = do
  (VBool res) <- isEqual v1 v2
  return . VBool $ not res

evalExp :: Expr -> Result Value
evalExp (EVar i) = getValueByIdent i
evalExp (ELitInt i) = return $ VNum i
evalExp ELitTrue = return $ VBool True
evalExp ELitFalse = return $ VBool False
evalExp (EApp i es) = undefined -- TODO: Assign
evalExp (EString s) = return $ VStr s
evalExp (ELambda args t b) = undefined -- TODO design
evalExp (Neg e) = do
  val <- evalExp e
  case val of
    (VNum v) -> return $ VNum (- v)
    _ -> throwError $ "Cannot change sign of a value of type " ++ show val
evalExp (Not e) = do
  val <- evalExp e
  case val of
    (VBool v) -> return $ VBool (not v)
    _ -> throwError $ "Cannot negate a value of type " ++ show val
evalExp (EAdd e1 op e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  evalAddOp op v1 v2
evalExp (EMul e1 op e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  evalMulOp op v1 v2
evalExp (ERel e1 op e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  evalRelOp op v1 v2
evalExp (EAnd e1 e2) = do
  (VBool val1) <- evalExp e1
  if not val1
    then return . VBool $ False
    else evalExp e2
evalExp (EOr e1 e2) = do
  (VBool val1) <- evalExp e1
  if val1
    then return . VBool $ True
    else evalExp e2