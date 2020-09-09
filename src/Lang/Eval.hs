module Lang.Eval (
    TermEnv,
    Value (..),
    evalExpr,
    evalDef,
    emptyTermEnv
) where

------------------------------------------------------------------------
--              Well-typed program cannot go wrong                    --
------------------------------------------------------------------------

{-
    This module is responsible for evaluating expressions (ast) and
    binding names to values in updated typing environment.

    Inidentally, it also defines what we understand as a value :)
-}

import qualified Data.Map               as Map

import           Control.Monad
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader
import           Lang.Syntax

----------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------

type TermEnv = Map.Map String Value

emptyTermEnv :: TermEnv
emptyTermEnv = Map.empty

-- |Evaluate expression within passed environment.
evalExpr :: TermEnv -> Expr -> Value
evalExpr = flip $ runReader . eval

-- |For evaluating top level definition and binding it to specified name.
evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (name, expr) =
  let v = evalExpr env expr
    in extendEnv env (name, v)

-- |Every typechecked expression evaluates to a value.
data Value
    = VInt Integer
    | VBool Bool
    | VClosure { argName :: String, body :: Expr, ctx :: TermEnv }
    deriving (Eq, Ord, Show)

----------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------

-- |Eval monad.
type EvalT a = Reader TermEnv a

-- |We need that in order to perform non-exhaustive pattern matching.
instance MonadFail Identity where
    fail = fail

-- |Behold evaluation logic ->
--      Quite simple since we know that well-typed program cannot go wrong.
eval :: Expr -> EvalT Value
eval expr = case expr of
    -- Whenever we encounter reference to some variable x
    -- we pull it out of the current context.
    (Var name) ->
        getValue name

    -- We instantiate literal value
    (Lit lit)  ->
        return $ instantiateLiteral lit

    -- We create closure with current context.
    (Lam name body) -> do
        ctx <- ask
        return $ VClosure name body ctx

    -- Bind name in closure to value of latter expression and evaluate first one.
    (App e1 e2) -> do
        VClosure name body ctx' <- eval e1
        v2 <- eval e2
        inModified ctx' name v2 $ eval body

    -- We apply binary operation
    (Op binop left right) -> do
        let f = op binop
        v1 <- eval left
        v2 <- eval right
        return $ f v1 v2

    -- We evaluate expression in extended environement.
    (Let argName e1 e2) -> do
        v1 <- eval e1
        inExtended argName v1 (eval e2)

    -- Check predicate and evaluate either left or right branch.
    (If predE trBranchE flBranchE) -> do
        VBool bool <- eval predE
        case bool of
            True  -> eval trBranchE
            False -> eval flBranchE

    -- We evaluate expression by applying the same expression to itself.
    -- This alone is enaugh to support recursion!
    Fix expr -> do
        eval $ App expr (Fix expr)

-- |Maps binary operation to function on values.
op :: BinOp -> (Value -> Value -> Value)
op binop = case binop of
    Add -> plus
    Sub -> minus
    Mul -> mult
    Eql -> eql
    where
        liftIntOp f con (VInt i1) (VInt i2) = con $ f i1 i2

        plus = liftIntOp (+) (VInt)

        minus = liftIntOp (-) (VInt)

        mult = liftIntOp (*) (VInt)

        eql = liftIntOp (==) VBool

-- |Maps Literal to Value.
instantiateLiteral :: Literal -> Value
instantiateLiteral (LInt i)  = VInt i
instantiateLiteral (LBool b) = VBool b

-- |Pulls value out of the eval context by name.
getValue :: String -> EvalT Value
getValue name = ask >>= return . (flip (Map.!)) name

-- |Execute action in current environment extend by key-value pair.
inExtended :: String -> Value -> EvalT a -> EvalT a
inExtended name val = local (Map.insert name val)

-- |Execute action in given environement extend by key-value pair.
inModified :: TermEnv -> String -> Value -> EvalT a -> EvalT a
inModified ctx' name value action =
    local (\_ -> Map.insert name value ctx') action

-- |Bind name to value and return modified environment.
extendEnv :: TermEnv -> (String, Value) -> TermEnv
extendEnv env (name, value)= Map.insert name value env

----------------------------------------------------------------------
----------------------------------------------------------------------
