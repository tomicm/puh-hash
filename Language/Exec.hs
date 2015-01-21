module Language.Exec where

import qualified Data.Map as M
import Language.Expressions
import Language.Commands

import qualified System.Process as P
import qualified System.IO as IO

type Command = [String] -> VarTable -> IO VarTable
type VarTable = M.Map String String

emptyState = M.empty

execStmt :: Stmt -> VarTable -> IO VarTable
execStmt (Cmd name args input output) vTable = do
  case M.lookup (evalExpr vTable name) builtinCmd of
    Just cmd -> do
      cmd (map (evalExpr vTable) args)
      return vTable
  
    Nothing -> do
      inHandle <- case input of
        Nothing  -> return IO.stdin
        Just exp -> IO.openFile (evalExpr vTable exp) IO.ReadMode
      outHandle <- case output of
        Nothing  -> return IO.stdout
        Just exp -> IO.openFile (evalExpr vTable exp) IO.WriteMode

      let process = P.proc (evalExpr vTable name) (map (evalExpr vTable) args)
      (_,_,_,pid) <- P.createProcess process {  P.std_in = P.UseHandle inHandle
                                             , P.std_out = P.UseHandle outHandle }

      P.waitForProcess pid

      return vTable

execStmt (Assign var val) vTable = do
  return $ nvartable
  where nvartable = M.insert var (evalExpr vTable val) vTable

execStmt (If pred cthen Nothing) vTable = do
  case (evalPred vTable pred) of
    True  -> execStmt cthen vTable
    False -> return vTable

execStmt (If pred cthen (Just celse)) vTable = do
  case (evalPred vTable pred) of
    True  -> execStmt cthen vTable
    False -> execStmt celse vTable

execStmt loop@(While pred cdo) vTable = do
  case (evalPred vTable pred) of
    True -> do
      nvTable <- execStmt cdo vTable
      execStmt loop nvTable
    False -> return vTable

execStmt (CmdL []) vTable = return vTable
execStmt (CmdL (st:sts)) vTable = do
  nvTable <- execStmt st vTable
  execStmt (CmdL sts) nvTable

evalComp :: VarTable -> Comp -> Bool
evalComp vTable (CEQ e1 e2) = (evalExpr vTable e1) == (evalExpr vTable e2)
evalComp vTable (CNE e1 e2) = (evalExpr vTable e1) /= (evalExpr vTable e2)
evalComp vTable (CGE e1 e2) = (evalExpr vTable e1) >= (evalExpr vTable e2)
evalComp vTable (CGT e1 e2) = (evalExpr vTable e1) >  (evalExpr vTable e2)
evalComp vTable (CLE e1 e2) = (evalExpr vTable e1) <= (evalExpr vTable e2)
evalComp vTable (CLT e1 e2) = (evalExpr vTable e1) <  (evalExpr vTable e2)

evalPred :: VarTable -> Pred -> Bool
evalPred vTable (Pred c   )  = evalComp vTable c
evalPred vTable (Parens p )  = evalPred vTable p
evalPred vTable (Not p    )  = not (evalPred vTable p)
evalPred vTable (And p1 p2)  = (evalPred vTable p1) && (evalPred vTable p2)
evalPred vTable (Or  p1 p2)  = (evalPred vTable p1) || (evalPred vTable p2)

evalExpr :: VarTable -> Expr -> String
evalExpr vTable (Str str) = str
evalExpr vTable (Var var) = 
  case M.lookup var vTable of
  Nothing  -> ""
  Just str -> str
evalExpr vTable (ExpList list) = 
  concat $ map (evalExpr vTable) list
