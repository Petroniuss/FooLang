module Lang.Syntax where

---------------------- Syntax -------------------------------------

type Name = String

data Expr
    = Var Name
    | App Expr Expr
    | Lam Name Expr
    | Let Name Expr Expr
    | Lit Literal
    | If Expr Expr Expr
    | Fix Expr
    | Op BinOp Expr Expr

    deriving (Show, Eq, Ord)

data Literal
    = LInt Integer
    | LBool Bool
    deriving (Show, Eq, Ord)

data BinOp = Add | Sub | Mul | Eql
    deriving (Eq, Ord, Show)

type Decl = (String, Expr)

data Program = Program [Decl] Expr deriving Eq

---------------------- Types -------------------------------------

-- Type variable example: a
newtype TVar =  TV String
    deriving (Eq, Show, Ord)

data Type
    = TVar TVar -- Type variable
    | TCon String -- Ground type
    | TArr Type Type -- Function type
    deriving (Eq, Show, Ord)

infixr `TArr`

-- Example: foralll a b. a -> b -> Int
data Scheme = Forall [TVar] Type
    deriving (Eq, Show, Ord)

-- Ground types
typeInt = TCon "Int"
typeBool = TCon "Bool"
