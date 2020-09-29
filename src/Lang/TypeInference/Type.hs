
module Lang.TypeInference.Type where

import           Lang.Syntax

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- Type variable example: a
newtype TypeVar = TypeVar String
    deriving (Eq, Show, Ord)

data Type
    = TVar TypeVar      -- Type variable - polymorphic types
    | TCon String       -- Ground type
    | TArr Type Type    -- Function type
    deriving (Eq, Show, Ord)

infixr `TArr`

-- Example: forall a b => a -> b -> Int
data TypeScheme = Forall [TypeVar] Type
    deriving (Eq, Show, Ord)

-- Ground types
typeInt = TCon "Int"
typeBool = TCon "Bool"

------------------------------------------------------------------------
-- Type Errors
------------------------------------------------------------------------

type Constraint = (Type, Type)

data TypeError
    -- Variable is not present in env
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
    -- When shapes of given types don't match and so we cannot make substitution.
    | UnificationMismatch [Type] [Type]


------------------------------------------------------------------------
------------------------------------------------------------------------
