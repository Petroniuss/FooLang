module Lambda.Typed.Syntax where

type Name = String

data Expr
  = Var Name
  | Lit Literal
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Show)

data Literal
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)