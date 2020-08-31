module Untyped.Eval where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Map
import Untyped.Syntax

type Context = Map String Value

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr Context
  deriving (Show)

data EvalState = EvalState
  { depth :: Int
  }
  deriving (Show)

type Step = (Int, Expr)

type Eval a = WriterT [Step] (State EvalState) a

inc :: Eval a -> Eval a
inc ev = do
  modify $ \s -> s {depth = (depth s) + 1}
  result <- ev
  modify $ \s -> s {depth = (depth s) - 1}
  return result

red :: Expr -> Eval ()
red expr = do
  d <- gets depth
  tell [(d, expr)]
  return ()

extend :: Context -> String -> Value -> Context
extend env n v = insert n v env

apply :: Value -> Value -> Eval Value
apply (VClosure n e clo) ex = do
  eval (extend clo n ex) e
apply _ _ = error "Tried to apply non-closure"

eval :: Context -> Expr -> Eval Value
eval ctx expr = case expr of
  Lit (LInt x) -> do
    return $ VInt (fromIntegral x)
  Lit (LBool x) -> do
    return $ VBool x
  Var name -> do
    red expr
    return $ ctx ! name
  Lam name body -> do
    return $ VClosure name body ctx
  App a b -> inc $ do
    x <- eval ctx a
    red a
    y <- eval ctx b
    red b
    apply x y

emptyCtx :: Context
emptyCtx = empty

runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyCtx x)) (EvalState 0)
