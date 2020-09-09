{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lang.Infer where

import           Lang.Substitution
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
import           Lang.Utils.Util


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


type Constraint = (Type, Type)

-- We generate constraints using stateful generation of new names
type Infer a = RWST TypeEnv [Constraint] [String] (Except TypeError) a


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

typeNamesSupply :: [String]
typeNamesSupply = [1..] >>= flip replicateM ['a'..'z']

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



-- There are some bugs because we don't introduce new types in place of those already in env

-- Two solutions here:
-- We might keep names supply in Shell monad but I don't like that..
-- Other thing we might do -> rename all types whenever we bring them from type env.


freshTypeVars :: [TypeVar] -> Infer Subsitution
freshTypeVars =
    flip foldM emptySubst $
        \acc e -> do
            var <- freshType
            return $ extendSubst e var acc

instantiateSchema :: TypeScheme -> Infer Type
instantiateSchema schema@(Forall vars tpe) = do
    map <- freshTypeVars vars
    return $ substitute map tpe

infer :: Expr -> Infer Type
infer expr =
    case expr of
        Var name -> do
            env <- ask
            case TypeEnv.lookup env name of
                Nothing     -> throwError $ UnboundVariable name
                Just schema -> instantiateSchema schema

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


-- evalInfer :: Expr -> TypeEnv -> (Type, [Constraint])
evalInfer :: Expr -> TypeEnv -> Either TypeError (Type, [Constraint])
evalInfer expr env = runExcept $ evalRWST (infer expr) env typeNamesSupply


-- Constraint solver

type SolverState = (Subsitution, [Constraint])
type ConstraintSolver a = StateT SolverState (Except TypeError) a

emptySolverState :: (Subsitution, [Constraint])
emptySolverState = (emptySubst, [])

occursCheck :: Substitutable a => TypeVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

bind :: TypeVar -> Type -> ConstraintSolver SolverState
bind var tp | tp == TVar var     = return emptySolverState
            | occursCheck var tp = throwError $ InifiniteType var tp
            | otherwise          = return $ (Map.singleton var tp, [])

unify :: Type -> Type -> ConstraintSolver SolverState
unify t1 t2                           | t1 == t2 = return (emptySubst, [])
unify (TVar x) t                      = x `bind` t
unify t (TVar x)                      = x `bind` t
unify (t1 `TArr` t2) (t1' `TArr` t2') = unifyMany [t1, t2] [t1', t2']
unify t1 t2                           = throwError $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> ConstraintSolver SolverState
unifyMany [] [] = return emptySolverState
unifyMany (t1: ts1) (t2: ts2) = do
    (s1, cs1) <- unify t1 t2
    (s2, cs2) <- unifyMany (substitute s1 ts1) (substitute s1 ts2)
    return (s2 `compose` s1, cs1 ++ cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

solve :: ConstraintSolver Subsitution
solve = do
    (su, cs) <- get
    case cs of
        [] -> return su
        ((t1, t2): cst) -> do
            (s1, cs1) <- unify t1 t2
            put (s1 `compose` su, cs1 ++ (substitute s1 cst))
            solve

runSolve :: [Constraint] -> Either TypeError Subsitution
runSolve cs = runExcept $ evalStateT solve (emptySubst, cs)

inferIt :: TypeEnv -> Expr -> Either TypeError TypeScheme
inferIt env expr = do
    (t, cs) <- evalInfer expr env
    subs    <- runSolve cs
    let t' = substitute subs t
    return $ generalize env t'

inferDecl :: TypeEnv -> (String, Expr) -> Either TypeError TypeEnv
inferDecl env (name, expr) = do
    (t, cs) <- evalInfer expr env
    subs    <- runSolve cs
    let t' = substitute subs t
    return $ extend env name $ generalize env t'

inferModule :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferModule env xs = foldM inferDecl env xs

