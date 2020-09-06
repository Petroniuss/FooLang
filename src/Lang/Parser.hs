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

import           Lang.Lexer
import           Lang.Syntax
import           Text.ParserCombinators.Parsec.Error (Message (Message),
                                                      addErrorMessage)

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

fix :: Parser Expr
fix = do
  reservedOp "fix"
  x <- expr
  return (Fix x)

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

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

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

-- If you are carfeul enaugh you will see that we have ciruclar dependencies..
-- But as this is lazy haskell this is completelty leigit!
term :: Parser Expr
term =
  try $ parens expr
  <|> try bool
  <|> try number
  <|> try ifthen
  <|> try fix
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
      binary "==" (Op Eql) Ex.AssocLeft
    ]
  ]

-- In case this looks confusing:
-- Parse expression and if it's possible to parse more expressions fold over them yielding tree of applications
-- else return parsed expr
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

parseExpr :: L.Text -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input

-- Returns list of top level definitons together with identifiers and some value to interpret
parseModule ::  FilePath -> L.Text -> Either ParseError ([(String, Expr)], Maybe Expr)
parseModule fname input = prepare <$> parse (contents modl) fname input
  where
  prepare :: [Binding] -> ([Binding], Maybe Expr)
  prepare xs =
    let its = filter ((==valId) . fst) xs
        others = filter ((/= valId) . fst) xs in
      case (reverse its) of
        []       -> (others, Nothing)
        (last:_) -> (others, (Just . snd) last)
