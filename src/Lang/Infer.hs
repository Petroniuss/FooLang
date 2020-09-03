{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lang.Infer where

import           Lang.Syntax

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State


import           Data.Monoid

import           Control.Monad.Identity (Identity)
import qualified Data.Map               as Map
import qualified Data.Set               as Set

data TypeError
    = UnboundVariable String

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

inifiniteNamesSupply :: [String]
inifiniteNamesSupply = [1..] >>= flip replicateM ['a'..'z']

--------------------------------------------------

type Infer a = ExceptT TypeError (NamesSupply) a
type TypeEnv = Map.Map Name Scheme
type Subst = Map.Map TVar Type


-- We're going to perform subsitution over the structure replacing type variables as specified
-- in subsitution

emptySubst :: Subst
emptySubst = Map.empty

-- I'm gonna need a second to comprehend this line
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
    -- Applies given substitution over the structure
    apply :: Subst -> a -> a
    -- Returns set of all free type variables of this structure
    ftv   :: a -> Set.Set TVar


instance Substitutable Type where
    apply sbst t = case t of
        tVar@(TVar x) -> Map.findWithDefault tVar x sbst

        tCon@(TCon _) -> tCon

        t1 `TArr` t2  -> (apply sbst t1) `TArr` (apply sbst t2)


    ftv t = case t of
        (TVar tvar)    -> Set.singleton tvar

        (TCon _)       -> Set.empty

        (t1 `TArr` t2) -> (ftv t1) `Set.union` (ftv t2)


instance Substitutable Scheme where
    -- I don't know why this is done this way>?>
    apply sbst (Forall tvars t) = Forall tvars $ apply s' t
        where s' = foldr Map.delete sbst tvars

    ftv (Forall tvars t) = (ftv t) `Set.difference` (ftv t)


instance Substitutable a => Substitutable [a] where
    apply = fmap . apply

    ftv xs = foldr (Set.union . ftv) Set.empty xs


instance Substitutable TypeEnv where
    apply = Map.map . apply

    ftv = ftv . Map.elems



