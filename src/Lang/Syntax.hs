module Lang.Syntax where

------------------------------------------------------------------------
--              Syntax for FooLang
------------------------------------------------------------------------

{-
    Module defines our abstract syntax tree and type system.

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
-- Types
------------------------------------------------------------------------

-- Type variable example: a
newtype TypeVar = TypeVar String
    deriving (Eq, Show, Ord)

data Type
    = TVar TypeVar      -- Type variable - polymorphic types
    | TCon String       -- Ground type
    | TArr Type Type    -- Function type
    deriving (Eq, Show, Ord)

infixr `TArr`

-- Example: forall a b => a -> b -> Int
data TypeScheme = Forall [TypeVar] Type
    deriving (Eq, Show, Ord)

-- Ground types
typeInt = TCon "Int"
typeBool = TCon "Bool"

------------------------------------------------------------------------
------------------------------------------------------------------------
