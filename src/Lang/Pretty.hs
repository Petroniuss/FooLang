{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lang.Pretty where

import           Control.Monad                             (replicateM)
import           Control.Monad                             (foldM)
import           Control.Monad.Identity                    (Identity,
                                                            runIdentity)
import           Control.Monad.Trans.State                 (StateT, evalStateT,
                                                            get, put)
import qualified Data.Map                                  as Map
import qualified Data.Set                                  as Set
import           Data.Text.Prettyprint.Doc                 (Doc, Pretty, align,
                                                            annotate, emptyDoc,
                                                            line, pretty, sep,
                                                            vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (Blue, Green, Red),
                                                            bold, color, putDoc)
import           Lang.Eval
import           Lang.Syntax
import           Lang.TypeEnv
import           Lang.TypeInference.Infer
import           Lang.TypeInference.Substitution
import           Lang.TypeInference.Type


------------------------------------------------------------------------
--              Type Inference
------------------------------------------------------------------------

{-
    This module is responsible for styling repl.

    It provides pretty instances for every printable type and
        interface for rendering.
-}

------------------------------------------------------------------------
-- Pretty typeclass instances
------------------------------------------------------------------------

instance Pretty Value where
    pretty value = case value of
        VInt i          -> pretty i

        VBool b         -> pretty b

        VClosure { .. } -> pretty "$$lambda$$"

instance Pretty TypeVar where
    pretty (TypeVar s) = pretty s

instance Pretty Type where
    pretty = prettyType

instance Pretty TypeScheme where
    pretty = prettyScheme

instance Pretty TypeError where
    pretty = prettyTypeError

instance Pretty TypeEnv where
    pretty = prettyEnv

-- | For printing evaluated expression.
prettyIt :: Value -> TypeScheme -> Doc ann
prettyIt v scheme =
    (pretty v) <+> (pretty "::") <+> prettyScheme scheme

-- |Pretty defintion with name and type.
prettyDecl :: String -> Type -> Doc ann
prettyDecl name tp = pretty name <+> (prettyType tp)

-- |Styling typescheme with identifier in front of it.
prettyNamedScheme :: String -> TypeScheme -> Doc ann
prettyNamedScheme name scheme =
    pretty name <+> pretty "::" <+> schemeDoc
        where schemeDoc = prettyScheme scheme

-- |Error for calling unbound function.
prettyDefNotFound :: String -> Doc ann
prettyDefNotFound name =
    pretty ("Definition `":: String) <>
    pretty name <>
    pretty ("` not found" :: String)

------------------------------------------------------------------------
-- Interface for rendering
------------------------------------------------------------------------

renderFailure :: Doc AnsiStyle -> IO ()
renderFailure doc = renderWithStyle  failureStyle doc

renderSuccess :: Doc AnsiStyle -> IO ()
renderSuccess doc = renderWithStyle successStyle doc

renderWithStyle :: AnsiStyle -> Doc AnsiStyle -> IO ()
renderWithStyle style doc = putDoc $ annotate style (doc <> line)

------------------------------------------------------------------------
-- Styles
------------------------------------------------------------------------

successStyle :: AnsiStyle
successStyle = color Blue <> bold

failureStyle :: AnsiStyle
failureStyle = color Red <> bold

------------------------------------------------------------------------
-- Instances' implementations
------------------------------------------------------------------------

-- |For browsing entire environment.
prettyEnv :: TypeEnv -> Doc ann
prettyEnv env =
    vsep docs
    where
        docs = map mapper $ Map.toList env
        mapper ((name, scheme)) = prettyNamedScheme name scheme


-- |Styling type schemes.
-- Note that beside simply styling we're also renaming type variables - it looks much nicer that way.
prettyScheme :: TypeScheme -> Doc ann
prettyScheme schema =
    let (Forall tvars tp) = freshSchema schema
        d = case (tvars) of
                [] -> pretty ""
                _  -> pretty "forall" <+>
                        (align . sep . (map pretty) $ reverse tvars) <+>
                        pretty "=> "
        in d <> prettyType tp
    where
        name :: StateT [String] Identity TypeVar
        name = do
            (x:xs) <- get
            put xs
            return $ TypeVar $ x

        freshSchema :: TypeScheme -> TypeScheme
        freshSchema (Forall vars tp) =
            let
                (subst, vars') = freshVars vars
                tp' = substitute subst tp
                in Forall vars' tp'

        alphabet = [1..] >>= flip replicateM ['a'..'z']

        freshVars vars =
            runIdentity $ flip evalStateT alphabet $
                foldM (\(subst, vars) e -> do
                    var <- name
                    let tp = TVar var
                    return $ (extendSubst e tp subst, var : vars))
                    (emptySubst, []) vars


-- |Styling types.
prettyType :: Type -> Doc ann
prettyType tp = align $ sep $
    zipWith (<>) (pretty "" : (repeat $ pretty "-> ")) (tys tp)
    where
        tys :: Type -> [Doc ann]
        tys tp = case tp of
                TVar (TypeVar x) -> [pretty x]

                TCon x           -> [pretty x]

                TArr t1 t2       -> tys t1 ++ tys t2


-- |Styling type errors.
prettyTypeError :: TypeError -> Doc ann
prettyTypeError tpErr = case tpErr of
    UnboundVariable name ->
        pretty $ "Variable `" <> name <> "` is unbound."

    UnificationFail t1 t2 ->
        pretty "Type mismatch" <> line <+>
        pretty t1 <> line <>
        pretty "     with" <> line <+>
        pretty t2

    InifiniteType (TypeVar name) t ->
        pretty "Infinite type: subsituting " <+>
        pretty t <+> pretty "for" <+> pretty t <+>
        pretty "would result in an infinite type"

    Ambigious constraints ->
        pretty "There is no unique solution for following set of constraints" <+>
        line <> cs
            where cs = mapMismatch constraints

    UnificationMismatch ts ts' ->
        pretty "Unification failed for following constraints" <+> line <> ms
            where ms = mapMismatch $ zip ts ts'

    -- Maps not fulfilled constraints to TypeError
    where mapMismatch = vsep .
                        map pretty .
                        map (\(t1, t2) -> UnificationFail t1 t2)

------------------------------------------------------------------------
------------------------------------------------------------------------
