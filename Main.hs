import Data.Text hiding (map)
import System.Environment

import ReadFSM
import RunFSM

main :: IO()
main = do
  args <- getArgs
  parseArgs args

parseArgs :: [String] -> IO()
parseArgs [] = printUsage
parseArgs [filename] = runOnString filename ""
parseArgs (filename : string : _) = runOnString filename string

runOnString :: String -> String -> IO()
runOnString filename string = do
  fsm <- loadFSM filename
  print $ runFSM fsm $ map (\ char -> pack [char]) string

printUsage :: IO()
printUsage =
  let
    message =
      "Usage:\n" ++
      "\t./Main <filename> (<string>)\n" ++
      "Parameters:\n" ++
      "\tfilename: the name of the .fsm file to load and run\n" ++
      "\tstring: the string to be evaluated by the machine\n"
  in
    putStr message
