import Control.Exception
import Control.Monad
import System.Directory
import System.Environment

main = do
  files <- getArgs
  if length files == 0 
    then do
      contents <- getContents
      putStrLn contents
    else do
      forM files $ \file -> do
        result <- try (readFile file) :: IO (Either SomeException String)
        case result of 
          Left ex   -> putStrLn $ show ex
          Right str -> putStr str
      return ()
