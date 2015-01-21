module Language.Expressions where

data Stmt = Cmd { name   :: Expr
                , args    :: [Expr]
                , input  :: Maybe Expr
                , output :: Maybe Expr
                }
          | Assign { var    :: String
                   , val    :: Expr
                   }
          | CmdL [Stmt]
          | If { pred  :: Pred
               , cThen :: Stmt
               , cElse :: Maybe Stmt
               }
          | While { pred :: Pred
                  , cdo  :: Stmt
                  }
          deriving (Show)

data Expr = Str String
          | Var String
          | ExpList [Expr]
            deriving (Eq, Show)

data Arg = Argument Expr 
         | InputRedir Expr
         | OutputRedir Expr
        deriving (Eq, Show)

data Pred = Pred Comp
          | Not Pred
          | Or Pred Pred
          | And Pred Pred
          | Parens Pred
            deriving (Eq, Show)

data Comp = CEQ Expr Expr -- ==
          | CNE Expr Expr -- /=
          | CGE Expr Expr -- >=
          | CGT Expr Expr -- >
          | CLE Expr Expr -- <=
          | CLT Expr Expr -- <
            deriving (Eq, Show)
