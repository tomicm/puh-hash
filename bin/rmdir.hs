import Control.Exception
import Control.Monad
import System.Directory
import System.Environment

main = do
  args <- getArgs

  -- rmdir takes at least one argument
  if null args
  then error "missing operand"
  else return ()

  forM args $ \dir -> do
    -- try to remove each directory and print some stuff it doesn't work
    res <- try (removeDirectory dir) :: IO (Either SomeException ())
    case res of
      Left e  -> putStrLn $ "rmdir: " ++ show e
      Right _ -> return () 
