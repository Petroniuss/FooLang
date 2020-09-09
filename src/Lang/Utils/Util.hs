module Lang.Utils.Util where


import           Control.Monad.Identity (Identity)

------------------------------------------------------------------------
--              Util
------------------------------------------------------------------------

{-
    This module holds reusable pieces of code shared across whole codebase.
-}

-- |We need that in order to perform non-exhaustive pattern matching.
instance MonadFail Identity where
    fail = error "Identity Monad failed!?"
