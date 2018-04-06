import System.Environment

import FSM
import ReadFSM
import RunFSM

main :: IO()
main = do
  args <- getArgs
  parseArgs args

parseArgs :: [String] -> IO()
parseArgs [] = printUsage
parseArgs [filename] = runFSMOnString filename ""
parseArgs (filename : string : _) = runFSMOnString filename string

runFSMOnString :: String -> String -> IO()
runFSMOnString filename string = do
  fsm <- loadFSM filename
  case fsm of
    Nothing ->
      print ("Invalid FSM specified in " ++ filename)
    Just actFSM ->
      print $ runFSM actFSM $ map Sym string

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
