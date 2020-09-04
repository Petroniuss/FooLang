module Lang.Env where

-- Our typing environment

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State

import qualified Data.Map               as Map

import           Data.List              (foldl')
import           Lang.Syntax

-- Our typing environment is simply wrapper over map
--    Expression identifier to its type scheme:
--      forall []   . Int -> Int
--      forall ["a"]. a -> Int
type Env = Map.Map String TypeScheme

emptyEnv :: Env
emptyEnv = Map.empty

singleton :: Env -> String -> TypeScheme -> Env
singleton env name scheme = Map.insert name scheme env

join :: [Env] -> Env
join = foldl' merge emptyEnv

merge :: Env -> Env -> Env
merge env1 env2 = env1 `Map.union` env2

extend :: Env -> String -> TypeScheme -> Env
extend env name scheme = Map.insert name scheme env

lookup :: Env -> String -> Maybe TypeScheme
lookup env name = Map.lookup name env


-- This is how we get new name for each type variable --
instance MonadFail Identity where
    fail = fail

freshName :: StateT [String] Identity String
freshName = do
    (name:others) <- get
    put others
    return name


inifiniteNamesSupply :: [String]
inifiniteNamesSupply = [1..] >>= flip replicateM ['a'..'z']
--------------------------------------------------
