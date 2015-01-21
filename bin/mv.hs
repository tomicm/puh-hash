import Control.Applicative
import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath

main = do
  args <- getArgs

  -- mv takes at least two arguments
  when (length args < 2) $ do
    error "missing operands"
    return ()

  let source = init args
  let target = last args

  case length source > 1 of
    True  -> do
  -- if there is more than 1 thing to be moved, the target
  -- has to be a directory 
      whenM (not <$> doesDirectoryExist target) $ do
        error $ "target `" ++ target ++ "` is not a directoy"
        return ()

  -- try to move each thing to the target directory
      forM source $ \src -> do
        result <- try (moveToDir src target Nothing) :: IO (Either SomeException ())
        case result of
          Left ex -> putStrLn $ show ex
          Right _ -> return ()
      return ()

    False -> do
  -- if there is just 1 thing to be moved, and the target 
  -- is a directory, then we move src to target
      whenM (doesDirectoryExist target) $
        moveToDir (source!!0) target Nothing

  -- if there is just 1 thing to be moved, and the target is not a 
  -- directory or doesn't exist at all, we rename src to target
      whenM (not <$> doesDirectoryExist target) $ do
        moveToDir (source!!0) "." (Just target)

moveToDir :: FilePath -> FilePath -> Maybe FilePath -> IO ()
moveToDir src dir firstName = do
  -- check if source exists
  whenM (not <$> doesAnythingExist src) $ do
    error $ "cannot move `" ++ src ++ "`: no such file or directory"
    return ()

  -- some special case handling
  let srcName = case firstName of
                  Just name -> name
                  Nothing   -> takeFileName src

  -- if src is a file, then we move it to the dir
  whenM (doesFileExist src) $ do
    copyFile src (dir </> srcName)
    removeFile src
    return ()

  -- if src is a directory, we move it to target
  -- and then we move all of its contents to target
  whenM (doesDirectoryExist src) $ do
    createDirectoryIfMissing False (dir </> src)
    contents <- getDirectoryContents' src
    forM contents $ \name -> do
      moveToDir (src </> name) (dir </> srcName) Nothing
      removeDirectory src
    return ()

  return ()

-- some helper functions
whenM s r = s >>= flip when r
doesAnythingExist name = liftA2 (||) (doesFileExist name) (doesDirectoryExist name)
getDirectoryContents' name = liftM (filter (`notElem` [".", ".."])) (getDirectoryContents name)
