module Parsing.HashParser where

import Language.Expressions

import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), (<|>), Applicative)
import Data.Char (digitToInt, isSpace)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator (sepBy, between, many1)
import Text.Parsec (parse, ParseError)
import Text.Parsec.Prim (many)

-- fails on characters from list and on whitespace
noneOfWhtSpc cs = satisfy (\c -> and [notElem c cs, not $ isSpace c])

-- variable identifier consists of digits, letters and 
-- underscores, but it cannot start with a digit
identifier :: Parser String
identifier = do
  c  <- char '_' <|> letter
  cs <- many $ char '_' <|> alphaNum 
  return (c:cs)

-- parses a quoted or unquoted string
someString :: Parser Expr
someString = quotedString <|> unquotedString

-- parses a quoted string
-- characters with special meaning: $, \, "
quotedString :: Parser Expr 
quotedString = do
  str <- between (char '"') (char '"') (many (variable <|> quotedStrPart))
  return $ if length str == 1 then head str else ExpList str

quotedStrPart :: Parser Expr
quotedStrPart = 
  Str <$> many1 (noneOf ['\\', '"', '$'] <|> (char '\\' *> anyChar))

-- parses an unquoted string
-- unquoted string cannot contain whitespace
-- characters with special meaning: $, \, "
unquotedString :: Parser Expr
unquotedString = do
  str <- many1 (variable <|> unquotedStrPart)
  return $ if length str == 1 then head str else ExpList str

unquotedStrPart :: Parser Expr
unquotedStrPart =
  Str <$> many1 (noneOfWhtSpc ['\\', '"', '$'] <|> (char '\\' *> anyChar))

-- parsers an expression, it can be a string or variable value
expression =   variable

-- variable value, like $mirko or $_slavko 
variable :: Parser Expr
variable = Var <$> (char '$' *> identifier)

-- parses a single command
command :: Parser Cmd
command =   Cmd
        <$> someString <* spaces 
        <*> sepBy someString spaces


