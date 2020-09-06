module Lang.Syntax where

---------------------- Syntax -------------------------------------

data Expr
    = Var String
    | App Expr Expr
    | Lam String Expr
    | Let String Expr Expr
    | Lit Literal
    | If Expr Expr Expr
    | Fix Expr
    | Op BinOp Expr Expr
--
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
newtype TypeVar = TypeVar String
    deriving (Eq, Show, Ord)

data Type
    = TVar TypeVar -- Type variable
    | TCon String -- Ground type
    | TArr Type Type -- Function type
    deriving (Eq, Show, Ord)

infixr `TArr`

-- Example: forall a b. a -> b -> Int
data TypeScheme = Forall [TypeVar] Type
    deriving (Eq, Show, Ord)

-- Ground types
typeInt = TCon "Int"
typeBool = TCon "Bool"
