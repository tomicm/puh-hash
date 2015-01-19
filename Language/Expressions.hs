module Language.Expressions where

data Cmd = Cmd { name   :: Expr
               , args   :: [Expr]
               }
          deriving (Show)

data Expr = Str String
          | Var String
          | ExpList [Expr]
            deriving (Eq, Show)
