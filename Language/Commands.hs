module Language.Commands where

import Language.Expressions

import qualified Data.Map as M
import qualified System.Directory as D

builtinCmd = 
  M.fromList [
    ("cd", changeDir)
  ]

changeDir :: [String] -> IO () 
changeDir args = do
  case null args of
    True -> do
      home <- D.getHomeDirectory
      D.setCurrentDirectory home
    False -> D.setCurrentDirectory $ head args
