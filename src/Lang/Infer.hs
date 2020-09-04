{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lang.Infer where
    -- ( Constraint
    -- , TypeError(..)
    -- , Subst(..)
    -- , inferTop
    -- , constraintsExpr
    -- )

import           Lang.Syntax

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Monoid

import           Control.Monad.Identity (Identity)
import qualified Data.Map               as Map
import qualified Data.Set               as Set


-- We're going to collect set of constraints from our ast and
-- then apply substiutions as specified in algorithm which will yield our final
-- solution

data TypeError
    -- Variable is not preseant in env
    = UnboundVariable String
    -- We couldn't have unified these two types.
    -- Thrown when we can't make any substiution - even an empty one.
    | UnificationFail Type Type
    -- This is subtle.
    -- We throw this one when subsitution would result in an infinite type.
    -- Example: unifying typevar a and type a -> b
    | InifiniteType TypeVar Type


-- type Infer a = ExceptT TypeError (NamesSupply) a
type TypeEnv = Map.Map String TypeScheme

---------------- Substitution --------------------
-- type Subst = Map.Map TVar Type

-- Unifying
-- Unify (TypeVar alpha) with (Type a) -> Subst [alpha -> a]
-- BUT: alpha cannot occur free in a bcs if it did we'd construct an infnite type:
-- Example:
--      (TypeVar "a1") `unify` (Type $ (TVar (TypeVar "a1") `TArr` (TVar (TypeVar "a2"))))
-- If we were to use our (Uni-VarLeft) rule we'd construct infinite type.

-------------------------------------------------


-- Generate set of constraints
-- infer :: Expr -> Infer Type
