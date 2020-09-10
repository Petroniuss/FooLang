{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lang.TypeInference.Infer where

import           Lang.Syntax
import           Lang.TypeEnv                        as TypeEnv
import           Lang.TypeInference.Substitution

import           Lang.TypeInference.Type

import           Lang.TypeInference.ConstraintSolver
import           Lang.TypeInference.ConstraintWriter

import           Control.Monad                       (foldM)
import           Lang.Utils.Util


------------------------------------------------------------------------
--              Type Inference
------------------------------------------------------------------------

{-
    This module is a single interface for Type Inference.
-}

------------------------------------------------------------------------
-- Interface
------------------------------------------------------------------------

-- | Infers type of entered expression.
inferIt :: TypeEnv -> Expr -> Either TypeError TypeScheme
inferIt env expr = do
    (t, cs) <- evalConstraintWriter expr env
    subs    <- runSolve cs
    let t' = substitute subs t
    return $ generalize env t'

-- | Infers type for whole module and returns modified typing environment.
inferModule :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferModule env xs = foldM inferDecl env xs

-- | Infers type for single definition and returns modified typing environment.
inferDecl :: TypeEnv -> (String, Expr) -> Either TypeError TypeEnv
inferDecl env (name, expr) = do
    (t, cs) <- evalConstraintWriter expr env
    subs    <- runSolve cs
    let t' = substitute subs t
    return $ extend env name $ generalize env t'


------------------------------------------------------------------------
------------------------------------------------------------------------
