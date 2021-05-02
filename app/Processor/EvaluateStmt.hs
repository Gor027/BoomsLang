module Processor.EvaluateStmt where

import Control.Monad.Except
import Control.Monad.RWS.Class
import Grammar.Abs
import Processor.Evaluate

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
evalMulOp Mod v1 v2 = throwError $ "Cannot find mod on types " ++ show v1 ++ " and " ++ show v2

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

checkStackOverflow :: Ident -> Result ()
checkStackOverflow ident = do
  (mem, addr, stackLevel) <- get
  if stackLevel > 5000 -- Stack size
    then throwError $ "Stack overflow on function " ++ show ident
    else -- Increment stack level to simulate function registration
      put (mem, addr, stackLevel + 1)

evalExp :: Expr -> Result Value
evalExp (EVar i) = getValueByIdent i
evalExp (ELitInt i) = return $ VNum i
evalExp ELitTrue = return $ VBool True
evalExp ELitFalse = return $ VBool False
evalExp (EApp ident args) = do
  checkStackOverflow ident -- throws "Stack Overflow Exception"
  (VFun (FnDef _ _ fArgs fBlock) _) <- getValueByIdent ident
  -- Note: function has type "FnDef Type Ident [Arg] Block"
  let fArgIdents = map (\(Arg _ argId) -> argId) fArgs -- get args
  let evaledArgs = map evalExp args -- evaluate args
  let Block stmts = fBlock
  continueDecl <- declareValues fArgIdents evaledArgs
  resReturned <- continueDecl $ evalBlock stmts
  case resReturned of
    Just val -> return val
    Nothing -> return VVoid
evalExp (EString s) = return $ VStr s
evalExp (ELambda args t b) = do
  VFun (FnDef t lambdaIdent args b) <$> ask
evalExp (Neg e) = do
  val <- evalExp e
  case val of
    (VNum v) -> return $ VNum (- v)
    _ -> throwError $ "Numeric negation applied to a non numberic value of type " ++ show val
evalExp (Not e) = do
  val <- evalExp e
  case val of
    (VBool v) -> return $ VBool (not v)
    _ -> throwError $ "Boolean negation applied to a non boolean value of type " ++ show val
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

defineFunctions :: Item -> Result Value
defineFunctions (Init _ exp) = do
  case exp of
    ELambda args t block -> do
      (VFun (FnDef funType funIdent funArgs funBlock) funEnv) <- evalExp (ELambda args t block)
      return $ VFun (FnDef funType funIdent funArgs funBlock) funEnv
    _ -> evalExp exp
defineFunctions (NoInit ident) = throwError $ "There is no definition for function with name " ++ show ident

defineValues :: Type -> Item -> Result Value
defineValues _ (Init _ exp) = evalExp exp
defineValues t (NoInit _) = defaultInit t

initItems :: [Item] -> Type -> [Result Value]
initItems items t = do
  case t of
    FuncType _ _ -> map defineFunctions items
    _ -> map (defineValues t) items

-- TODO: Implement block evaluation
evalBlock :: [Stmt] -> Result (Maybe Value)
evalBlock (s : ss) =
  case s of
    Empty -> evalBlock ss
    BStmt (Block stmts) -> do
      resultBlock <- evalBlock stmts
      case resultBlock of
        Nothing -> evalBlock ss
        Just val -> return $ Just val
    Decl t items -> do
      -- int x, y, z; || fun (int -> int) fib, timesTwo;
      let defValues = initItems items t
      let idents = declareIdents items
      -- get function `Result (Maybe a) -> Result (Maybe a)` to continue declaration of items
      continueDecl <- declareValues idents defValues
      continueDecl $ evalBlock ss
