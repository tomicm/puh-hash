import Control.Exception
import Control.Monad
import System.Directory
import System.Environment

main = do
  args <- getArgs

  if null args
    then error "missing operand"
    else return ()

  forM args $ \name -> do
  -- try to remove all the arguments, and print stuff if anything goes wrong
    res <- try (removeFile name) :: IO (Either SomeException ())
    case res of
      Left ex -> putStrLn $ show ex
      Right _ -> return ()
