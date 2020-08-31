{-# LANGUAGE RecordWildCards #-}

module Lambda.Typed.Eval where

import Control.Monad.State.Lazy
import Data.Map.Strict
import Lambda.Typed.Syntax
import Prelude hiding (id)

type Context = Map Name Value

extendCtx :: (Name, Value) -> Eval ()
extendCtx (k, v) = do
  ctx <- get
  put $ insert k v ctx

getValue :: Context -> Name -> Value
getValue = (!)

fresh :: Context
fresh = empty

data Value
  = VClosure {name :: Name, body :: Expr, ctx :: Context}
  | VInt {vInt :: Int}
  | VBool {vBool :: Bool}
  deriving (Show)

literalValue :: Literal -> Value
literalValue (LInt i) = VInt i
literalValue (LBool b) = VBool b

type EvalState = Context

type EvalResult = Value

type Eval a = State EvalState a

eval :: Expr -> Eval Value
eval Lit {..} = do
  return $ literalValue literal
eval Var {..} = do
  ctx <- get
  return $ getValue ctx id
eval Lam {..} = do
  ctx <- get
  return $ VClosure arg body ctx
eval App {..} = do
  closure <- eval f
  body <- eval x
  apply closure body

-- It's not correct actually
-- Applies the closure
apply :: Value -> Value -> Eval Value
apply (VClosure name body ctx) arg = do
  extendCtx (name, arg)
  eval body
apply _ _ = error "Tried to apply noclosure"

runEval :: Expr -> Value
runEval expr = evalState (eval expr) fresh
