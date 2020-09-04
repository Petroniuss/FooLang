{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lang.Infer where 
    ( Constraint
    , TypeError(..) 
    , Subst(..)
    , inferTop
    , constraintsExpr
    )

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
    -- Thrown when we can't make any substiution - even empty one
    | UnificationFail Type Type
    -- This is subtle.
    -- We throw this one when subsitution would result in an infinite type.
    | InifiniteType TVar Type

-- Fresh names for our types

type NamesSupply = State [Name]

-- This is how we get new name for each variable --
instance MonadFail Identity where
    fail = fail

freshName :: NamesSupply Name
freshName = do
    (name:others) <- get
    put others
    return name

fresh :: Infer Type
fresh = do
    name <- lift $ freshName
    return $ TVar $ TV name

inifiniteNamesSupply :: [String]
inifiniteNamesSupply = [1..] >>= flip replicateM ['a'..'z']
--------------------------------------------------

type Infer a = ExceptT TypeError (NamesSupply) a
type TypeEnv = Map.Map Name Scheme

---------------- Substitution --------------------
type Subst = Map.Map TVar Type

