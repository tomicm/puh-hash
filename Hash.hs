module Hash (runInteractive, runScript) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Language.Exec
import Language.Expressions
import Parsing.HashParser
import Network.BSD

import System.IO
import System.Directory
import System.Environment

executeHashRc :: IO VarTable
executeHashRc = do
  homeDir <- getHomeDirectory
  exists <- doesFileExist (homeDir ++ "/.hashrc")
  if exists
    then runScriptWithRet emptyState (homeDir ++ "/.hashrc")
    else return emptyState

runInteractive :: IO ()
runInteractive = do
  sState <- executeHashRc
  mainLoop sState

runScript :: FilePath -> IO ()
runScript script = do
  vTable <- executeHashRc
  runScriptWithRet vTable script
  return ()

runScriptWithRet :: VarTable -> FilePath -> IO VarTable
runScriptWithRet vTable script = do
  src <- readFile script
  lineByLine (lines src) vTable

lineByLine :: [String] -> VarTable -> IO VarTable 
lineByLine [] vTable = return vTable

lineByLine (l:ls) sState = do
  let stmt = parseLine (l ++ ";")

  nextState <- case stmt of
    Left _ -> return sState
    Right x -> do
      result <- try (execStmt x sState) :: IO (Either SomeException VarTable)
      case result of
        Left ex   -> do
          putStrLn $ show ex
          return sState
        Right res -> return res

  lineByLine ls nextState

mainLoop sState = do
    wkDir <- getCurrentDirectory
    hmDir <- getHomeDirectory
    username <- getEnv "USERNAME"
    hostname <- getHostName
    putStr $ "\027[1;34m" ++ username ++ "@" ++ hostname
    putStr $ " \027[1;35m" ++ wkDir ++ " "
    putStr "λ \027[0m"
    hFlush stdout
    
    line <- getLine
    let stmt = parseLine (line ++ ";")
    nextState <- case stmt of
      Left _ -> return sState
      Right x -> do
        result <- try (execStmt x sState) :: IO (Either SomeException VarTable)
        case result of
          Left ex   -> do
            putStrLn $ show ex
            return sState
          Right res -> return res
    mainLoop nextState
