import Hash
import System.Environment

main = do
  args <- getArgs

  if null args
  then runInteractive 
  else runScript (args!!0)
