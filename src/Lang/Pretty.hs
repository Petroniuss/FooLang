{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Lang.Pretty where

import           Data.Map                                  (toList)
import           Data.Text.Prettyprint.Doc                 (Doc, Pretty, align,
                                                            annotate, emptyDoc,
                                                            line, pretty, sep,
                                                            vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (Blue, Green, Red),
                                                            bold, color, putDoc)
import           Lang.Eval
import           Lang.Infer
import           Lang.Syntax
import           Lang.TypeEnv


successStyle :: AnsiStyle
successStyle = color Blue

failureStyle :: AnsiStyle
failureStyle = color Red <> bold

renderFailure :: Doc AnsiStyle -> IO ()
renderFailure doc = renderWithStyle  failureStyle doc

renderSuccess :: Doc AnsiStyle -> IO ()
renderSuccess doc = renderWithStyle successStyle doc

renderWithStyle :: AnsiStyle -> Doc AnsiStyle -> IO ()
renderWithStyle style doc = putDoc $ annotate style (doc <> line)

prettyIt :: Pretty a => a -> TypeScheme -> Doc ann
prettyIt v scheme =
    (pretty v) <+> (pretty "::") <+> prettyScheme scheme

prettyDefNotFound :: String -> Doc ann
prettyDefNotFound name =
    pretty ("Definition `":: String) <>
    pretty name <>
    pretty ("` not found" :: String)

prettyDecl :: String -> Type -> Doc ann
prettyDecl name tp = pretty name <+> (prettyType tp)

prettyEnv :: TypeEnv -> Doc ann
prettyEnv env =
    vsep docs
    where
        docs = map mapper $ toList env
        mapper ((name, scheme)) = prettyNamedScheme name scheme

prettyNamedScheme :: String -> TypeScheme -> Doc ann
prettyNamedScheme name scheme =
    pretty name <+> pretty "::" <+> schemeDoc
        where schemeDoc = prettyScheme scheme

prettyScheme :: TypeScheme -> Doc ann
prettyScheme (Forall tvars tp) =
    let d = case tvars of
                [] -> pretty ""
                _  -> pretty "forall" <+>
                        (align . sep . (map pretty) $ tvars) <+>
                        pretty "=> "
        in d <> prettyType tp

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


prettyType :: Type -> Doc ann
prettyType tp = align $ sep $
    zipWith (<>) (pretty "" : (repeat $ pretty "-> ")) (tys tp)
    where
        tys :: Type -> [Doc ann]
        tys tp = case tp of
                TVar (TypeVar x) -> [pretty x]

                TCon x           -> [pretty x]

                TArr t1 t2       -> tys t1 ++ tys t2


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

    -- Maps constraints to UnificationFail
    where mapMismatch = vsep . map pretty . map (\(t1, t2) -> UnificationFail t1 t2)

