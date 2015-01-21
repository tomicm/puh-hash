import Control.Monad
import Data.Maybe
import System.Directory

main = do
  wd <- getCurrentDirectory
  putStrLn $ wd

