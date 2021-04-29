module Processor.EvaluateStmt where

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

data Op = AddOp | MulOp deriving (Show, Read, Eq, Ord)

-- TODO: Relational operations should be evaluated here
evalOp :: Op -> Value -> Value -> Result Value
evalOp Plus (VNum v1) (VNum v2) = return $ VNum $ v1 + v2
evalOp Plus (VStr v1) (VStr v2) = return $ VStr $ v1 ++ v2
evalOp Plus v1 v2 = throwError $ "Cannot add values of type " ++ show v1 ++ " and " ++ show v2
evalOp Minus (VNum v1) (VNum v2) = return $ VNum $ v1 - v2
evalOp Minus v1 v2 = throwError $ "Cannot subtract values of type " ++ show v1 ++ " and " ++ show v2
evalOp Times (VNum v1) (VNum v2) = return $ VNum $ v1 * v2
evalOp Times v1 v2 = throwError $ "Cannot multiply values of type " ++ show v1 ++ " and " ++ show v2
evalOp Div (VNum v1) (VNum v2)
  | v2 == 0 = throwError "Cannot divide by 0"
  | otherwise = return $ VNum $ div v1 v2
evalOp Div v1 v2 = throwError $ "Cannot divide values of type " ++ show v1 ++ " and " ++ show v2
evalOp Mod (VNum v1) (VNum v2)
  | v2 == 0 = throwError "Cannot find remainder for division by 0"
  | otherwise = return $ VNum $ mod v1 v2

evalExp :: Expr -> Result Value
evalExp (EVar i) = getValueByIdent i
evalExp (ELitInt i) = return $ VNum i
evalExp ELitTrue = return $ VBool True
evalExp ELitFalse = return $ VBool False
evalExp (EApp i es) = undefined
evalExp (EString s) = return $ VStr s
evalExp (ELambda args t b) = undefined
evalExp (Neg e) = do
  val <- VNum (evalExp e)
  return $ VNum (- val)
evalExp (Not e) = do
  val <- VBool (evalExp e)
  return $ VBool $ not val
evalExp (EAdd e1 op e2) = liftM2 (evalOp op) (evalExp e1) (evalExp e2)
evalExp (EMul e1 op e2) = liftM2 (evalOp op) (evalExp e1) (evalExp e2)
