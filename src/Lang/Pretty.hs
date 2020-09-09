{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Lang.Pretty where

import           Data.Map                                  (toList)
import           Data.Text.Prettyprint.Doc                 (Doc, Pretty, align,
                                                            annotate, emptyDoc,
                                                            line, pretty, sep,
                                                            vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle,
                                                            Color (Green, Red),
                                                            bold, color, putDoc)
import           Lang.Eval
import           Lang.Syntax
import           Lang.TypeEnv


successStyle = color Green
failureStyle = color Red <> bold

render :: Doc AnsiStyle -> IO ()
render doc = putDoc $ annotate successStyle $ (doc <> line)

renderWithStyle style doc = putDoc $ annotate style (doc <> line)

prettyIt v scheme =
    (pretty v) <+> (pretty "::") <+> prettyScheme scheme

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


prettyType :: Type -> Doc ann
prettyType tp = align $ sep $
    zipWith (<>) (pretty "" : (repeat $ pretty "-> ")) (tys tp)
    where
        tys :: Type -> [Doc ann]
        tys tp = case tp of
                TVar (TypeVar x) -> [pretty x]

                TCon x           -> [pretty x]

                TArr t1 t2       -> tys t1 ++ tys t2

