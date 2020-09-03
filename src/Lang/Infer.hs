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
    | UnificationFail Type Type
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

emptySubst :: Subst
emptySubst = Map.empty

-- I'm gonna need a second to comprehend this line
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = (Map.map (apply s1) s2) `Map.union` s1

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
    -- I apply modified substitution with all elements from tvars deleted, over the type
    apply sbst (Forall tvars t) = Forall tvars $ apply s' t
        where s' = foldr Map.delete sbst tvars

    ftv (Forall tvars t) = (ftv t) `Set.difference` (ftv t)


instance Substitutable a => Substitutable [a] where
    apply = fmap . apply

    ftv xs = foldr (Set.union . ftv) Set.empty xs


instance Substitutable TypeEnv where
    apply = Map.map . apply

    ftv = ftv . Map.elems


--------------------- Unification -----------------------------

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unify :: Type -> Type -> Infer Subst
unify (l `TArr` r) (l' `TArr` r') = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return $ s2 `compose` s1

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return emptySubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Infer Subst
bind a t | t == TVar a = return emptySubst
         | occursCheck a t = throwError $ InifiniteType a t
         | otherwise = return $ Map.singleton a t

---------------------------------------------------------------

---------------- Generealization and Instantiation ------------

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env


----------------- Typing Rules --------------------------------

lookupEnv :: TypeEnv -> Name -> Infer (Subst, Type)
lookupEnv env name = do
    case Map.lookup name env of
        Nothing -> throwError $ UnboundVariable (show name)
        Just s  -> do t <- instantiate s
                      return (emptySubst, t)

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend env (x, s) = Map.insert x s env

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty

typeof :: TypeEnv -> Name -> Maybe Scheme
typeof env name = Map.lookup name env

ops :: Map.Map BinOp Type
ops = Map.fromList [
    (Add, nArrow 3 typeInt),
    (Sub, nArrow 3 typeInt),
    (Mul, nArrow 3 typeInt),
    (Eql, (typeInt `TArr` (typeInt `TArr` typeBool)))]
        where
            nArrow :: Int -> Type -> Type
            nArrow 1 tpe = tpe
            nArrow n tpe = tpe `TArr` (nArrow (n - 1) tpe)

typeArrow :: Type -> Type -> Type -> Type
typeArrow t1 t2 t3 = t1 `TArr` (t2 `TArr` t3)


infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of
    Var x -> lookupEnv env x

    Lam x e -> do
        tv <- fresh
        let env' = env `extend` (x, Forall [] tv)
        (s1, t1) <- infer env' e
        return $ (s1, apply s1 tv `TArr` t1)

    App e1 e2 -> do
        tv <- fresh
        (s1, t1) <- infer env e1
        (s2, t2) <- infer (apply s1 env) e2

        s3       <- unify (apply s2 t1) (t2 `TArr` tv)
        return (s3 `compose` s2 `compose` s1, apply s3 tv)

    Let x e1 e2 -> do
        (s1, t1) <- infer env e1
        let env' = apply s1 env
            t'   = generalize env' t1
        (s2, t2) <- infer (env' `extend` (x, t')) e2
        return (s1 `compose` s2, t2)

    Op op e1 e2 -> do
        (s1, t1) <- infer env e1
        (s2, t2) <- infer env e2
        tv <- fresh
        s3 <- unify (typeArrow t1 t2 tv) (ops Map.! op)
        return (s1 `compose` s2 `compose` s3, apply s3 tv)







