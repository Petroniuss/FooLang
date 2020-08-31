module Lambda.Typed.Syntax where

type Name = String

data Expr
  = Var {id :: Name}
  | Lit {literal :: Literal}
  | App {f :: Expr, x :: Expr}
  | Lam {arg :: Name, body :: Expr}
  deriving (Eq, Show)

data Literal
  = LInt {lInt :: Int}
  | LBool {lBool :: Bool}
  deriving (Show, Eq, Ord)

data Type
  = TInt
  | TBool
  | TArr {from :: Type, to :: Type}
  deriving (Eq, Read, Show)
