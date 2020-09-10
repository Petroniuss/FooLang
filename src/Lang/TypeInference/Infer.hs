{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lang.TypeInference.Infer where

import           Lang.Syntax
import           Lang.TypeEnv                        as TypeEnv
import           Lang.TypeInference.Substitution

import           Lang.TypeInference.Type

import           Lang.TypeInference.ConstraintSolver
import           Lang.TypeInference.ConstraintWriter

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Monoid

import           Control.Monad.Identity              (Identity)
import           Control.Monad.RWS                   (RWST, ask, evalRWS,
                                                      evalRWST, local, tell)
import           Data.List                           (foldl')
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set
import           Lang.Utils.Util



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

-- Interface

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

