import Control.Exception
import Control.Monad
import System.Directory
import System.Environment

main = do
  args <- getArgs

  -- mkdir takes at least one argument
  if null args 
  then error "missing operand"
  else return ()

  forM args $ \dir -> do
  -- try to make some directory and print some stuff if anything goes wrong 
    res <- try (createDirectory dir) :: IO (Either SomeException ())
    case res of
      Left e  -> putStrLn $ "mkdir: " ++ show e
      Right _ -> return () 
