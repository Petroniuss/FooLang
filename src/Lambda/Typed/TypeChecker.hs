{-# LANGUAGE RecordWildCards #-}

module Lambda.Typed.TypeChecker where


import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Reader (Reader, ask, local, runReader)
import           Prelude              hiding (lookup)

import           Control.Monad.Except (MonadTrans (lift))
import           Control.Monad.Except (MonadError (throwError))
import           Data.Map.Lazy
import           Lambda.Typed.Syntax

data TypeError
    = NotInScope { arg :: Name }
    | TypeMismatch { expected :: Type, actual :: Type }
    | NotFunction { expectedType :: Type }


type Env = Map Name Type

type Check a = ExceptT TypeError (Reader Env) a


extend :: (Name, Type) -> Env  -> Env
extend (n, t) env  = insert n t env

lookupName :: Name -> Check Type
lookupName name = do
    env <- ask
    case (lookup name env) of
        Nothing -> throwError $ NotInScope name
        Just t  -> return t


check :: Expr -> Check Type
check expr = case expr of
    Var { .. } -> lookupName name

    Lit { .. } -> case literal of
        LInt { }   -> return $ TInt
        LBool {  } -> return $ TBool

    Lam { .. } -> do
        env <- ask
        let env' = extend (arg, argType)
        retType <- local (env') $ check body
        return $ TArr argType retType

    App { .. } -> do
        t1 <- check f
        t2 <- check x
        case t1 of
            (TArr { from = a, to = b}) | a == t2 -> return b
                                       | otherwise -> throwError $ TypeMismatch a t2
            _ -> throwError $ NotFunction t1

freshEnv :: Env
freshEnv = empty

runCheck :: Expr -> Either TypeError Type
runCheck expr = runReader getReader freshEnv
    where
        getReader = runExceptT $ check expr
