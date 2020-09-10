module Lang.Syntax where

------------------------------------------------------------------------
--              Syntax for FooLang
------------------------------------------------------------------------

{-
    Module defines our abstract syntax tree.

    Note that this language is very small,
    but on the other hand haskell's internal Core
    language is boiled down to 9 constructors.

    I suppose this is the most important module.
-}

------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------

data Expr
    = Var { id :: String }
    | App Expr Expr
    | Lam { arg :: String, body :: Expr }
    | Let { letName :: String, letExpr :: Expr, inExpr :: Expr }
    | Lit Literal
    | If { cond :: Expr, trueBranch :: Expr, falseBranch :: Expr }
    | Fix Expr -- Recursion!
    | Op BinOp Expr Expr
    deriving (Show, Eq, Ord)

data Literal
    = LInt Integer
    | LBool Bool
    deriving (Show, Eq, Ord)

data BinOp = Add | Sub | Mul | Eql
    deriving (Eq, Ord, Show)


------------------------------------------------------------------------
------------------------------------------------------------------------
