import Control.Exception
import Control.Monad
import System.Environment
import System.IO

main = do
  args <- getArgs
  
  -- touch requires at least one argument
  if null args
  then error "missing operand"
  else return ()

  -- touch everyone if appropriate
  forM args $ \filename -> do
   res <- try (appendFile filename "") :: IO (Either SomeException ())
   case res of
     Left ex -> putStrLn $ show ex
     Right _ -> return ()

