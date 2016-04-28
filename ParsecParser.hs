module ParsecParser (parseExpr) where

import Eval
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token


-- Lexer

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef {
          Token.reservedOpNames = ["->","\\","+","*","-","="],
          Token.reservedNames = ["let","in"],
          Token.commentLine = "#" }

natural :: Parser Integer
natural = Token.natural lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer


-- Parser

variable :: Parser Expression
variable = Var `fmap` identifier

number :: Parser Expression
number = (Const . fromIntegral) `fmap` natural

letin :: Parser Expression
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (App (Abs x e2) e1)

lambda :: Parser Expression
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp "->"
  e <- expr
  return (Abs x e)

expr :: Parser Expression
expr = letin <|> lambda <|> formula

formula :: Parser Expression
formula = buildExpressionParser [[mulOp],[addOp,subOp]] juxta <?> "formula"
  where addOp = Infix (reservedOp "+" >> return addExpr) AssocLeft
        subOp = Infix (reservedOp "-" >> return subExpr) AssocLeft
        mulOp = Infix (reservedOp "*" >> return mulExpr) AssocLeft

juxta :: Parser Expression
juxta = (foldl1 App) `fmap` (many1 atom)

atom :: Parser Expression
atom = variable <|> number <|> parens expr <?> "atom"

allOf :: Parser a -> Parser a
allOf p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Expression
parseExpr t =
  case parse (allOf expr) "stdin" t of
    Left err -> error (show err)
    Right ast -> ast
