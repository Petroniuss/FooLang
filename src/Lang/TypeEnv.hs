module Lang.TypeEnv where

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
type TypeEnv = Map.Map String TypeScheme

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty

singleton :: TypeEnv -> String -> TypeScheme -> TypeEnv
singleton env name scheme = Map.insert name scheme env

join :: [TypeEnv] -> TypeEnv
join = foldl' merge emptyTypeEnv

merge :: TypeEnv -> TypeEnv -> TypeEnv
merge env1 env2 = env1 `Map.union` env2

extend :: TypeEnv -> String -> TypeScheme -> TypeEnv
extend env name scheme = Map.insert name scheme env

lookup :: TypeEnv -> String -> Maybe TypeScheme
lookup env name = Map.lookup name env

--------------------------------------------------
