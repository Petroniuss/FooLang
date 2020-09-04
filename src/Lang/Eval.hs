module Lang.Eval where

 -- Since eval is so much easier I should begin with this guy :)
-- We don't care about errors since well-typed program cannot go wrong...

import qualified Data.Map               as Map

import           Control.Monad
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader
import           Lang.Syntax


-------------------------- TOP LEVEL ----------------------------------
evaluate :: TermCtx -> Expr -> Value
evaluate = flip $ runReader . eval

data Value
    = VInt Int
    | VBool Bool
    | VClosure { argName :: String, body :: Expr, ctx :: TermCtx }

-------------------------- TOP LEVEL ----------------------------------

type TermCtx = Map.Map String Value

type EvalT a = Reader TermCtx a

-- Pulls value out of the eval context by name
getValue :: String -> EvalT Value
getValue name = ask >>= return . (flip (Map.!)) name

-- Execute action in current environment extend by key-value pair
inExtended :: String -> Value -> EvalT a -> EvalT a
inExtended name val = local (Map.insert name val)

-- Execute action in given environement extend by key-value pair
inModified :: TermCtx -> String -> Value -> EvalT a -> EvalT a
inModified ctx' name value action =
    local (\_ -> Map.insert name value ctx') action


-- We need that in order to perform non-exhaustive pattern matching
instance MonadFail Identity where
    fail = fail

eval :: Expr -> EvalT Value
eval expr = case expr of
    -- Whenever we encounter reference to some variable x
    -- we pull it out of the current context
    (Var name) ->
        getValue name

    -- We instantiate literal value
    (Lit lit)  ->
        return $ instantiateLiteral lit

    -- We create closure with current context
    (Lam name body) -> do
        ctx <- ask
        return $ VClosure name body ctx

    -- Bind name in closure to value of latter expression and evaluate first one
    (App e1 e2) -> do
        VClosure name body ctx' <- eval e1
        v2 <- eval e2
        inModified ctx' name v2 $ eval e1

    -- We apply binary operation
    (Op binop left right) -> do
        let f = op binop
        v1 <- eval left
        v2 <- eval right
        return $ f v1 v2

    -- We evaluate expression in extended environement
    (Let name e1 e2) -> do
        v1 <- eval e1
        inExtended name v1 (eval e2)

    -- Check predicate and evaluate either left or right branch
    (If predE trBranchE flBranchE) -> do
        VBool bool <- eval predE
        case bool of
            True  -> eval trBranchE
            False -> eval flBranchE

-- Maps binary operation to function on values
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

-- Maps Literal to Value
instantiateLiteral :: Literal -> Value
instantiateLiteral (LInt i)  = VInt . fromIntegral $ i
instantiateLiteral (LBool b) = VBool b

