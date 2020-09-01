{-# LANGUAGE RecordWildCards #-}

module Lambda.Typed.Eval where

import           Control.Monad.State.Lazy
import           Data.Map.Strict
import           Lambda.Typed.Syntax
import           Prelude                  hiding (id)

type Context = Map Name Value

extendCtx :: Context -> (Name, Value) -> Context
extendCtx ctx (k, v) =
  insert k v ctx

getValue :: Context -> Name -> Value
getValue = (!)

fresh :: Context
fresh = empty

data Value
  = VClosure {name :: Name, body :: Expr, ctx :: Context}
  | VInt {vInt :: Int}
  | VBool {vBool :: Bool}

instance Show (Value) where
  show (VClosure {..}) = "<<closure>> "
  show VInt { .. }     = show vInt
  show VBool { .. }    = show vBool

literalValue :: Literal -> Value
literalValue (LInt i)  = VInt i
literalValue (LBool b) = VBool b

eval :: Context -> Expr -> Value
eval ctx Lit {..} =
  literalValue literal
eval ctx Var {..} =
  getValue ctx name
eval ctx Lam {..} =
  VClosure arg body ctx
eval ctx App {..} =
  let closure = eval ctx f
      arg = eval ctx x
   in apply closure arg

apply :: Value -> Value -> Value
apply (VClosure name body ctx) arg =
  let ctx' = extendCtx ctx (name, arg)
   in eval ctx' body
apply _ _ = error "Tried to apply non-closure"

runEval :: Expr -> Value
runEval = eval fresh
