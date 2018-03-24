import Data.Text hiding (map)
import System.Environment

import ReadFSM
import RunFSM

main :: IO()
main = do
  (filename : string : _) <- getArgs
  fsm <- loadFSM filename
  print $ runFSM fsm $ map (\ char -> pack [char]) string
