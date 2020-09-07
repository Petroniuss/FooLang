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
import           Lang.TypeEnv           as TypeEnv

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Monoid

import           Control.Monad.Identity (Identity)
import           Control.Monad.RWS      (RWST, ask, evalRWS, evalRWST, local,
                                         tell)
import           Data.List              (foldl')
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
    -- There's no unique solution to given set of constraints
    | Ambigious [Constraint]
    -- Duunot know :)
    | UnificationMismatch [Type] [Type]
    deriving (Show)



-- type Infer a = ExceptT TypeError (NamesSupply) a

---------------- Substitution --------------------
-- type Subst = Map.Map TVar Type

-- Unifying
-- Unify (TypeVar alpha) with (Type a) -> Subst [alpha -> a]
-- BUT: alpha cannot occur free in a bcs if it did we'd construct an infnite type:
-- Example:
--      (TypeVar "a1") `unify` (Type $ (TVar (TypeVar "a1") `TArr` (TVar (TypeVar "a2"))))
-- If we were to use our (Uni-VarLeft) rule we'd construct infinite type.

-------------------------------------------------

type Constraint = (Type, Type)

-- We generate constraints using stateful generation of new names
type Infer a = RWST TypeEnv [Constraint] [String] (Except TypeError) a

-- Generating type names
instance MonadFail Identity where
    fail = error "pattern matching failed"

freshType :: Infer Type
freshType = do
    var <- freshTypeVar
    return $ TVar var

freshTypeVar :: Infer TypeVar
freshTypeVar = do
    name <- freshName
    return $ TypeVar name

freshName :: Infer String
freshName = do
    (name:others) <- get
    put others
    return name

inifiniteNamesSupply :: [String]
inifiniteNamesSupply = [1..] >>= flip replicateM ['a'..'z']

----

addConstraint :: (Type , Type) -> Infer ()
addConstraint (t1,t2) = do tell [(t1, t2)]


locally :: (String, TypeScheme) -> Infer a -> Infer a
locally (name, scheme) inf = do
    let trans e = Map.insert name scheme e
    local trans inf


emptyScheme :: Type -> TypeScheme
emptyScheme var = Forall [] var


generalize :: TypeEnv -> Type -> TypeScheme
generalize tpEnv tpe = Forall vars tpe
    where vars = Set.toList $ ftv tpe `Set.difference` ftv tpEnv

----

type Subsitution = Map.Map TypeVar Type

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
    ftv (Forall vars tp) = ftv tp `Set.difference` Set.fromList vars

    substitute subs (Forall vars tp) = Forall vars $ substitute s' tp
                            where s' = foldr Map.delete subs vars

instance Substitutable TypeEnv where
    ftv env = foldr (Set.union . ftv) Set.empty $ Map.elems env

    substitute subs env = Map.map (substitute subs) env

infer :: Expr -> Infer Type
infer expr =
    case expr of
        Var name -> do
            env <- ask
            case TypeEnv.lookup env name of
                Nothing             -> throwError $ UnboundVariable name
                Just (Forall _ tpe) -> return tpe

        App e1 e2 -> do
            t1 <- infer e1
            t2 <- infer e2
            resultT <- freshType
            addConstraint (t1, t2 `TArr` resultT)
            return resultT

        Lam name expr -> do
            argT <- freshType
            res <- locally (name, emptyScheme argT) $ infer expr
            return (argT `TArr` res)

        Let name e1 e2 -> do
            t1 <- infer e1
            env <- ask
            let generalized = generalize env t1
            locally (name, generalized) $ infer e2

        Lit (LInt _)  -> return typeInt

        Lit (LBool _) -> return typeBool

        If condE trE flE -> do
            t1 <- infer condE
            t2 <- infer trE
            t3 <- infer flE
            addConstraint (t1, typeBool)
            addConstraint (t2, t3)
            return t2

        Fix e1 -> do
            t1 <- infer e1
            tv <- freshType
            addConstraint (tv `TArr` tv, t1)
            return tv

        Op binop e1 e2 -> do
            t1 <- infer e1
            t2 <- infer e2
            resT <- freshType
            let actual = t1 `TArr` t2 `TArr` resT
                expected = opType binop

            addConstraint (actual, expected)
            return resT

opType :: BinOp -> Type
opType Add = typeInt `TArr` typeInt `TArr` typeInt
opType Sub = typeInt `TArr` typeInt `TArr` typeInt
opType Mul = typeInt `TArr` typeInt `TArr` typeInt
opType Eql = typeInt `TArr` typeInt `TArr` typeBool


-- Interface
-- evalInfer :: Expr -> TypeEnv -> (Type, [Constraint])
evalInfer :: Expr -> TypeEnv -> Either TypeError (Type, [Constraint])
evalInfer expr env = runExcept $ evalRWST (infer expr) env inifiniteNamesSupply

-- I just wanna run this guy to see what we have so far

-- Constraint solver

-- type UnifierState = (Subsitution, [Constraint])
-- type Unifier a    = StateT UnifierState (Except TypeError) a

inferModule :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferModule env xs = foldM foo env xs
    where
        foo acc (name, expr) = do
             (t, _) <- evalInfer expr acc
             return $ extend acc name (emptyScheme t)

