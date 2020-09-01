module Lambda.Typed.Syntax where

type Name = String

data Expr
  = Var {name :: Name}
  | Lit {literal :: Literal}
  | App {f :: Expr, x :: Expr}
  | Lam {arg :: Name, argType :: Type, body :: Expr}
  deriving (Eq, Show)

data Literal
  = LInt {lInt :: Int}
  | LBool {lBool :: Bool}
  deriving (Show, Eq, Ord)

data Type
  = TInt
  | TBool
  | TArr {from :: Type, to :: Type}
  deriving (Eq, Read)

instance Show Type where
  show (TInt)       = "Int"
  show (TBool)      = "Bool"
  show (TArr t1 t2) = (show t1) <> " -> " <> (show t2)
