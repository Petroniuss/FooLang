{-# LANGUAGE FlexibleInstances #-}
module Lang.Substitution where

-- Maybe we should have a separate module for substitution
import qualified Data.Map     as Map
import qualified Data.Set     as Set
import           Lang.Syntax
import           Lang.TypeEnv

type Subsitution = Map.Map TypeVar Type

-- This is how we compose substitutions
compose :: Subsitution -> Subsitution -> Subsitution
s1 `compose` s2 = Map.map (substitute s1) s2 `Map.union` s1

emptySubst :: Subsitution
emptySubst = Map.empty

extendSubst :: TypeVar -> Type -> Subsitution -> Subsitution
extendSubst = Map.insert

class Substitutable a where
    -- Returns set of all free type variables
    ftv :: a -> Set.Set TypeVar

    -- Replaces all occurences of free type variables with given type
    substitute :: Subsitution -> a -> a

instance Substitutable Type where

    ftv tp = case tp of
        TVar var   -> Set.singleton var

        TCon _     -> Set.empty

        TArr t1 t2 -> ftv t1 `Set.union` ftv t2

    substitute subs tp = case tp of
        TVar var -> case Map.lookup var subs of
                        Nothing -> tp
                        Just v  -> v
        t@(TCon _)   -> t

        t1 `TArr` t2 -> (substitute subs t1) `TArr` (substitute subs t2)


instance Substitutable TypeScheme where
    ftv (Forall vars tp) = (ftv tp) `Set.difference` (Set.fromList vars)

    substitute subs (Forall vars tp) = Forall vars $ substitute s' tp
                            where s' = foldr Map.delete subs vars

instance Substitutable TypeEnv where
    ftv env = ftv $ Map.elems env

    substitute subs env = Map.map (substitute subs) env

instance Substitutable a => Substitutable [a] where
    ftv xs = foldr (Set.union . ftv) Set.empty xs

    substitute subs xs = substitute subs <$> xs

instance (Substitutable a) => Substitutable (a, a) where
    ftv (x, y) = (ftv x) `Set.union` (ftv y)

    substitute subs (x, y) = (substitute subs x, substitute subs y)
