{-# LANGUAGE OverloadedStrings #-}

module Lang.Parser (
  parseExpr,
  parseModule
) where

import           Text.Parsec
import           Text.Parsec.Text.Lazy               (Parser)

import qualified Text.Parsec.Expr                    as Ex
import qualified Text.Parsec.Token                   as Tok

import qualified Data.Text.Lazy                      as L

import           Data.List                           (foldl')
import           Lang.Lexer
import           Lang.Syntax
import           Text.ParserCombinators.Parsec.Error (Message (Message),
                                                      addErrorMessage)

------------------------------------------------------------------------
--              Parser -- based on parsec
------------------------------------------------------------------------

{-
    Module responsible for parsing input (L.Text) and producing either ParseError or [Ast].

    If you are careful enaugh you will see that we have ciruclar between combinators..
    But as this is lazy haskell it is completelty leigit!
-}

------------------------------------------------------------------------
-- Interface
------------------------------------------------------------------------

type AST = ([(String, Expr)], Maybe Expr)

parseExpr :: L.Text -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input


parseModule ::  FilePath -> L.Text -> Either ParseError AST
parseModule fname input = prepare <$> parse (contents modl) fname input
  where
  prepare :: [Binding] -> AST
  prepare xs =
    let its = filter ((==valId) . fst) xs
        others = filter ((/= valId) . fst) xs in
      case (reverse its) of
        []       -> (others, Nothing)
        (last:_) -> (others, (Just . snd) last)

------------------------------------------------------------------------
-- Implementation
------------------------------------------------------------------------

integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- integer
  return (Lit (LInt (fromIntegral n)))

bool :: Parser Expr
bool = (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

-- We could possibly let define many bindings using let expresion
letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)


letrecin :: Parser Expr
letrecin = do
  reserved "let"
  reserved "rec"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

elseif :: Parser (Expr, Expr)
elseif = do
  reserved "else if"
  cond <- expr
  reservedOp "then"
  instructions <- expr
  return (cond, instructions)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  branches <- many elseif
  reserved "else"
  fl <- expr
  let
    branches' = (cond, tr) : branches
    ex = foldl' (\e (cnd, inst) -> If cnd inst e) fl $ reverse branches'
  -- or foldr
  -- let ex = foldr (\(cnd, inst) e -> If cnd inst e) fl branches'
  return ex

term :: Parser Expr
term =
  try $ parens expr
  <|> try bool
  <|> try number
  <|> try ifthen
  <|> try letrecin
  <|> try letin
  <|> try lambda
  <|> try variable


binary :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
binary x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
table = [
    [
      binary "*" (Op Mul) Ex.AssocLeft
    ],
    [
      binary "+" (Op Add) Ex.AssocLeft
    , binary "-" (Op Sub) Ex.AssocLeft
    ],
    [
      binary "==" (Op Eql) Ex.AssocLeft,
      binary "<" (Op Lt) Ex.AssocLeft,
      binary "<=" (Op Lte) Ex.AssocLeft,
      binary ">" (Op Gt) Ex.AssocLeft,
      binary ">=" (Op Gte) Ex.AssocLeft,
      binary "and" (Op And) Ex.AssocLeft,
      binary "or" (Op Or) Ex.AssocLeft
    ]
  ]

-- |In case this looks confusing:
-- Parse expression and if it's possible to parse more expressions,
-- fold over them yielding tree of applications
-- else return parsed expr.
expr :: Parser Expr
expr = do
  x <- Ex.buildExpressionParser table term
  (try $ do
    xs <- many1 term
    return (foldl App x xs))
    <|> return x


type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return $ (name, foldr Lam body args)

letrecdecl :: Parser (String, Expr)
letrecdecl = do
  reserved "let"
  reserved "rec"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return $ (name, Fix $ foldr Lam body (name:args))

valId :: String
valId = "value_to_be_interpreted"

val :: Parser Binding
val = do
  ex <- expr
  return (valId, ex)

decl :: Parser Binding
decl = try letrecdecl <|> try letdecl <|> val

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top


------------------------------------------------------------------------
------------------------------------------------------------------------
