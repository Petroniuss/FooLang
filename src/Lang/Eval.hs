module Lang.Eval where

-- Since eval is so much easier I should begin with this guy :)
-- We don't care about errors since well-typed program cannot go wrong...

import qualified Data.Map             as Map

import           Control.Monad.Reader
import           Lang.Syntax

type Ctx = Map.Map String Value

data Value
    = VInt Int
    | VBool Bool
    | VClosure { argName :: String, body :: Expr, ctx :: Ctx }

emptyCtx :: Ctx
emptyCtx = Map.empty

get :: Ctx -> String -> Value
get ctx name = (Map.!) ctx name

extend :: Ctx -> String -> Value -> Ctx
extend ctx name val = Map.insert name val ctx

literal :: Literal -> Value
literal (LInt i)  = VInt . fromIntegral $ i
literal (LBool b) = VBool b


type EvalT a = Reader Ctx

-- Actually we might store our ctx in readerT
evaluate :: Ctx -> Expr -> Value
evaluate ctx expr = case expr of

    (Var name) -> get ctx name

    (Lit lit)  -> literal lit

    (Op binop e1 e2) -> f v1 v2
        where v1 = evaluate ctx e1
              v2 = evaluate ctx e2
              f = op binop

    (Lam name body) -> VClosure name body ctx

    (Let name e1 e2) ->
        evaluate ctx' e2
        where v1 = evaluate ctx e1
              ctx' = extend ctx name v1

    (If pred trBranch flBranch) ->
        case (predValue $ evaluate ctx pred) of
            True  -> evaluate ctx trBranch
            False -> evaluate ctx flBranch

        where predV = evaluate ctx pred
              predValue (VBool b) = b
              predValue _         = error "Well-typed program cannot go wrong :)"

    (App e1 e2) ->
        apply closure
        where closure = evaluate ctx e1
              argV = evaluate ctx e2
              apply (VClosure name body ctx') =
                  let ctx'' = extend ctx' name argV
                    in evaluate ctx'' body


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
