import Control.Applicative
import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath

main = do
  args <- getArgs

  -- cp takes at least two arguments
  when (length args < 2) $ do
    error "missing operand"
    return ()

  let source = init args
  let target = last args

  case length source > 1 of
    True  -> do
    -- if there is more than 1 thing to be copied, the target 
    -- has to be an existing directory
      whenM (not <$> doesDirectoryExist target) $ do
        error $ "target `" ++ target ++ "` is not a directory"
        return ()

    -- try to copy each src to target directory
      forM source $ \src -> do
        res <- try (copyToDir src target Nothing) :: IO (Either SomeException ())
        case res of
          Left ex -> putStrLn $ show ex
          Right _ -> return ()

      return ()

    False -> do
    -- if there is just one thing to be copied, and target is
    -- a directoy, then we copy the thing inside the target
      whenM (doesDirectoryExist target) $ do
        copyToDir (source!!0) target Nothing
        return ()

    -- if target is not a directory or doesn't exist at all
    -- then we copy src, possibly overwriting the target
      whenM (not <$> doesDirectoryExist target) $ do
        copyToDir (source!!0) "." (Just target)
        return ()

copyToDir :: FilePath -> FilePath -> Maybe FilePath -> IO ()
copyToDir src dir firstName = do
  -- check if source file exists
  whenM (not <$> doesAnythingExist src) $ do
    error $ "cannot copy `" ++ src ++ "`: no such file or directory"
    return ()

  -- some special case handling
  let srcName = case firstName of
                  Just name -> name
                  Nothing   -> takeFileName src

  -- if src is a file, just copy it to target
  whenM (doesFileExist src) $ do
    copyFile src (dir </> srcName)
    return ()

  -- if src is a directory, copy it to target
  -- and then copy all of its contents into newly created dir
  whenM (doesDirectoryExist src) $ do
    createDirectoryIfMissing False (dir </> src)
    contents <- getDirectoryContents' src
    forM contents $ \name ->
      copyToDir (src </> name) (dir </> srcName) Nothing
    return ()

  return ()

-- some helper functions
whenM s r = s >>= flip when r
doesAnythingExist name = liftA2 (||) (doesFileExist name) (doesDirectoryExist name)
getDirectoryContents' name = liftM (filter (`notElem` [".", ".."])) (getDirectoryContents name)
