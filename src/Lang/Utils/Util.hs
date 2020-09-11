module Lang.Utils.Util where


import           Control.Monad.Identity (Identity)
import qualified Data.Text              as T

------------------------------------------------------------------------
--              Util
------------------------------------------------------------------------

{-
    This module holds reusable pieces of code shared across whole codebase.
-}

-- |We need that in order to perform non-exhaustive pattern matching.
instance MonadFail Identity where
    fail = error "Identity Monad failed!?"

-- |For removing trailing white space.
strip :: String -> String
strip = T.unpack . T.strip . T.pack

------------------------------------------------------------------------
------------------------------------------------------------------------
