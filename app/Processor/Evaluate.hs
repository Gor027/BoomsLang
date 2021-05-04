{-# LANGUAGE FlexibleContexts #-}

module Processor.Evaluate where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Grammar.Abs

import Debug.Trace as T

-- Setup

type Addr = Integer

type Env = M.Map Ident Addr

type Mem = M.Map Addr Value

type StackLevel = Integer

type Store = (Mem, Addr, StackLevel)

-- ExceptT is responsible for recording runtime errors
-- StateT holds the memory state with current memory cell and stack usage counter
-- ReaderT keeps program's immutable environment
-- IO is in the bottom of stack of monad transformers
type Result = ReaderT Env (StateT Store (ExceptT String IO))

data Value
  = VBool {bool :: Bool}
  | VNum {num :: Integer}
  | VStr {str :: String}
  | VFun {fun :: TopDef, env :: Env}
  | VVoid
  | VBreak
  | VCont
  deriving (Eq)

instance Show Value where
  show (VBool a) = show a
  show (VNum a) = show a
  show (VStr a) = a
  show (VFun f _) = show f
  show VVoid = "void"
  show VBreak = "break"
  show VCont = "continue"

lambdaIdent :: Ident
lambdaIdent = Ident "lambda"

-- End setup

newMCell :: Result Addr
newMCell = do
  (mem, addr, stCount) <- get
  put (mem, addr + 1, stCount)
  return addr

updateMem :: (Mem -> Mem) -> Result ()
updateMem f = modify (\(mem, addr, stCount) -> (f mem, addr, stCount))

getValueByAddr :: Addr -> Result Value
getValueByAddr addr = do
  (mem, _, _) <- get
  case M.lookup addr mem of
    Just val -> return val
    Nothing -> throwError "Nothing found in the specified address"

getValueByIdent :: Ident -> Result Value
getValueByIdent ident = do
  env <- ask
  case M.lookup ident env of
    Just addr -> getValueByAddr addr
    Nothing -> throwError $ "Unknown variable " ++ show ident

updateVarByIdent :: Ident -> (Value -> Value) -> Result ()
updateVarByIdent ident f = do
  env <- ask
  case M.lookup ident env of
    Just addr -> do
      val <- getValueByAddr addr
      updateMem $ M.insert addr (f val)
    Nothing -> throwError $ "Unknown variable " ++ show ident

defaultInit :: Type -> Result Value
defaultInit Int = return $ VNum 0
defaultInit Bool = return $ VBool False
defaultInit Str = return $ VStr ""
defaultInit _ = throwError "Types other than Int, Bool, Str are not initialized by default"

-- value declaration

declareValue :: Ident -> Result Value -> Result (Result a -> Result a)
declareValue name val = do
  cell <- newMCell
  vInit <- val
  let vAssign = case vInit of
        VFun f env -> VFun f (M.insert name cell env)
        _ -> vInit
  updateMem (M.insert cell vAssign)
  return (local (M.insert name cell))

declareValues :: [Ident] -> [Result Value] -> Result (Result a -> Result a)
declareValues [] [] = return (local id)
declareValues [] (_ : _) = throwError "Parameter mismatch"
declareValues (_ : _) [] = throwError "Parameter mismatch"
declareValues (var : vars) (val : values) = do
  declVar <- declareValue var val
  declVars <- declareValues vars values
  return $ declVar . declVars

declareIdents :: [Item] -> [Ident]
declareIdents = map getFromItem
  where
    getFromItem (NoInit i) = i
    getFromItem (Init i _) = i

-- end of value declaration

debugShow :: Result ()
debugShow = do
  e <- ask
  (mem,_, _) <- get
  let debugString = join $ map (entryToStr mem) (M.toList e)
  _ <- trace debugString $ return ()
  return ()
  where
    entryToStr :: Mem -> (Ident, Addr) -> String
    entryToStr mm (l, r) = "DEBUG: '" ++ identStr l ++ "'->" ++ show r ++ "->" ++ show (M.lookup r mm) ++ "\n"
    identStr (Ident i) = i

