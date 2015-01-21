module Parsing.HashParser where

import Language.Expressions

import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), (<|>), Applicative)
import Control.Monad
import Data.Char (digitToInt, isSpace)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator 
import Text.Parsec (parse, ParseError)
import Text.Parsec.Prim (many, try, unexpected)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr

language = 
  P.LanguageDef { P.commentStart        = ""
                , P.commentEnd          = ""
                , P.commentLine         = "#"
                , P.nestedComments      = False 
                , P.identStart          = letter <|> char '_'
                , P.identLetter         = alphaNum <|> char '_' 
                , P.opStart             = oneOf "" 
                , P.opLetter            = oneOf ""
                , P.caseSensitive       = True
                , P.reservedNames       = ["if", "then", "else", "while"
                                          , "done", "fi"
                                          ]
                , P.reservedOpNames     = ["and", "or", "not", ">", ">=", "<"
                                          , "<=", "/=", "=="
                                          ]
                }

lexer = P.makeTokenParser language

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme     lexer
semi       = P.semi       lexer
parens     = P.parens     lexer
reserved   = P.reserved   lexer
reservedOp = P.reservedOp lexer

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
someString = do
  str <- many1 (quotedString <|> unquotedString)
  return $ if length str == 1 then head str else ExpList str

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
-- characters with special meaning: $, \, ", >, <, ;
unquotedString :: Parser Expr
unquotedString = do
  str <- many1 (variable <|> unquotedStrPart)
  return $ if length str == 1 then head str else ExpList str

unquotedStrPart :: Parser Expr
unquotedStrPart =
  Str <$> many1 (   noneOfWhtSpc ['\\', '"', '$', '>', '<', ';'] 
                <|> (char '\\' *> anyChar)
                )

-- parses an expression, it can be a string
expression = someString

-- variable value, like $mirko or $_slavko 
variable :: Parser Expr
variable = Var <$> (char '$' *> identifier)

-- parse many semicolon separated statements
statements :: Parser Stmt
statements = do
  list <- many statement 
  return $ if length list == 1 then head list else CmdL list

-- parse many semicolon separated statements until a keyword
statementsTill :: String -> Parser Stmt
statementsTill str = do
  list <- manyTill statement (reserved str)
  return $ if length list == 1 then head list else CmdL list

-- parse a statement
statement :: Parser Stmt
statement =   try assign
          <|> try conditional
          <|> try whileLoop
          <|> command

-- parses a single command
command :: Parser Stmt
command = do
  name <- lexeme someString
  args <- many   cmdArg
  semi
  return $ Cmd name 
               (getRealArgs args) 
               (findInputRedir args) 
               (findOutputRedir args)


-- find last input/output redirection from arguments
findInputRedir :: [Arg] -> Maybe Expr
findInputRedir = foldr lastInputRedir Nothing
  where lastInputRedir (InputRedir exp) Nothing = Just exp
        lastInputRedir _                 curr    = curr

findOutputRedir :: [Arg] -> Maybe Expr
findOutputRedir = foldr lastOutputRedir Nothing
  where lastOutputRedir (OutputRedir exp) Nothing = Just exp
        lastOutputRedir _                 curr    = curr

-- throw out anything that isn't a real argument
getRealArgs :: [Arg] -> [Expr]
getRealArgs = (map unwrapArg) . (filter isArg)
  where isArg (Argument _) = True
        isArg _            = False
        unwrapArg (Argument exp) = exp
        unwrapArg _              = error "invalid input to unwrap argument"

-- parses a command argument
cmdArg :: Parser Arg
cmdArg =   try inputRedirect
       <|> try outputRedirect
       <|> argument 

-- input/output redirection and argument parsers
inputRedirect :: Parser Arg
inputRedirect = liftM InputRedir (reservedOp "<" *> lexeme someString)

outputRedirect :: Parser Arg
outputRedirect = liftM OutputRedir (reservedOp ">" *> lexeme someString)

argument :: Parser Arg
argument = Argument <$> lexeme someString

-- parses an assignment
assign :: Parser Stmt
assign =  do
  var <- identifier
  char '='
  val <- someString
  semi
  return $ Assign var val

-- parses a conditional
conditional :: Parser Stmt
conditional =   try ifThenElse
            <|> ifThen

ifThen = do
  reserved "if"
  pred <- predicate
  reserved "then"
  cthen <- statementsTill "fi"
  semi
  return $ If pred cthen Nothing

ifThenElse = do
  reserved "if"
  pred <- predicate
  reserved "then"
  cthen <- statementsTill "else"
  celse <- statementsTill "fi"
  semi
  return $ If pred cthen (Just celse)

-- parses a while loop
whileLoop :: Parser Stmt
whileLoop = do
  reserved "while"
  pred <- predicate
  reserved "do"
  cdo <- statementsTill "done"
  semi
  return $ While pred cdo

-- definition of boolean operators
bOperators = [ [Prefix (reservedOp "not" >> return (Not)) ]
             , [Infix  (reservedOp "and" >> return (And)) AssocLeft ]
             , [Infix  (reservedOp "or"  >> return (Or )) AssocLeft ]
             ]

-- parser for predicates
predicate :: Parser Pred
predicate = buildExpressionParser bOperators pTerm

-- parser for terms in predicates
pTerm :: Parser Pred
pTerm =   parens predicate
      <|> liftM Pred comparison

-- parser for comparison
comparison :: Parser Comp
comparison = do
  a1 <- lexeme expression
  op <- compOperator
  a2 <- lexeme expression
  return $ op a1 a2

-- parser for comparison operator
compOperator :: Parser (Expr -> Expr -> Comp)
compOperator =   (try $ reservedOp ">="  >> return CGE)
             <|> (reservedOp ">"   >> return CGT)
             <|> (try $ reservedOp "<="  >> return CLE)
             <|> (reservedOp "<"   >> return CLT)
             <|> (reservedOp "=="  >> return CEQ)
             <|> (reservedOp "/="  >> return CNE)

-- parses a line written in Hash
parseLine line = parse (whiteSpace >> statements) "" line
