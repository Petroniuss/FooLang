module Lang.TypeInference.ConstraintSolver where

import           Lang.Syntax
import           Lang.TypeEnv                        as TypeEnv
import           Lang.TypeInference.Substitution

import           Lang.TypeInference.Type

import           Control.Monad.Except                (Except, runExcept,
                                                      throwError)
import           Control.Monad.State                 (StateT, evalStateT, get,
                                                      put)
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set
import           Lang.TypeInference.ConstraintWriter

------------------------------------------------------------------------
--              Constraint Solver
------------------------------------------------------------------------

{-
    Module solving set of constraints yielding either Type Error
        or final Substitution.
-}

------------------------------------------------------------------------
-- Interface
------------------------------------------------------------------------

-- | Yields TypeError or Substitution.
runSolve :: [Constraint] -> Either TypeError Subsitution
runSolve cs = runExcept $ evalStateT solve (emptySubst, cs)

------------------------------------------------------------------------
-- Implementation
------------------------------------------------------------------------

type SolverState = (Subsitution, [Constraint])

type ConstraintSolver a = StateT SolverState (Except TypeError) a

emptySolverState :: (Subsitution, [Constraint])
emptySolverState = (emptySubst, [])

occursCheck :: Substitutable a => TypeVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- | Binds typevariable to given type making sure it does not lead to an infinite type.
-- For example constraint (typevar a) = type (a -> b) leads to such error.
bind :: TypeVar -> Type -> ConstraintSolver SolverState
bind var tp | tp == TVar var     = return emptySolverState
            | occursCheck var tp = throwError $ InifiniteType var tp
            | otherwise          = return $ (Map.singleton var tp, [])

-- | Unify two types <=> solve t1 = t2.
unify :: Type -> Type -> ConstraintSolver SolverState
unify t1 t2                           | t1 == t2 = return (emptySubst, [])
unify (TVar x) t                      = x `bind` t
unify t (TVar x)                      = x `bind` t
unify (t1 `TArr` t2) (t1' `TArr` t2') = unifyMany [t1, t2] [t1', t2']
unify t1 t2                           = throwError $ UnificationFail t1 t2

-- | Unifies arrow types.
unifyMany :: [Type] -> [Type] -> ConstraintSolver SolverState
unifyMany [] [] = return emptySolverState
unifyMany (t1: ts1) (t2: ts2) = do
    (s1, cs1) <- unify t1 t2
    (s2, cs2) <- unifyMany (substitute s1 ts1) (substitute s1 ts2)
    return (s2 `compose` s1, cs1 ++ cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

-- | General Overview of solver:
--      1. Pick one constraint
--      2. Unify it (Solve)
--      3. Add newly emerged constraints.
--      4. Compose substitutions.
--      4. Do it as long as there are any constraints left.
solve :: ConstraintSolver Subsitution
solve = do
    (su, cs) <- get
    case cs of
        [] -> return su
        ((t1, t2): cst) -> do
            (s1, cs1) <- unify t1 t2
            put (s1 `compose` su, cs1 ++ (substitute s1 cst))
            solve


------------------------------------------------------------------------
------------------------------------------------------------------------
