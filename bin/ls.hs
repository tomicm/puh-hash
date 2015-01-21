import Control.Exception
import Control.Monad
import Data.List
import System.Environment
import System.Directory

main = do
  argList <- getArgs

  -- if no arguments are provided, list the contents of current directory
  let args = if length argList == 0 then ["."] else argList

  forM (zip [1..] args) $ \(id, path) -> do
    result <- try (getDirectoryContents path) :: IO (Either SomeException [FilePath])
    case result of
      -- error, output the message
      Left exc          -> putStrLn $ show exc

      -- no error, list contents
      Right dirContents -> do
        -- some formatting stuff
        if id /= 1 then putStrLn "" else return ()

        -- sort the names alphabetically
        putStrLn $ unlines $ sort dirContents

  return ()
