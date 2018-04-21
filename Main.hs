import System.Environment

import FSM
import ReadFSM
import RunFSM
import Regex
import RegexToFSM

main :: IO()
main = do
  args <- getArgs
  parseArgs args

parseArgs :: [String] -> IO()
parseArgs [] = printUsage
parseArgs [filename] = runFSMOnString filename ""
parseArgs ("-r" : regex : []) = runRegexOnString regex ""
parseArgs ("-r" : regex : string : _) = runRegexOnString regex string
parseArgs (filename : string : _) = runFSMOnString filename string

printUsage :: IO()
printUsage =
  let
    message =
      "Usage:\n" ++
      "\t./Main <filename> (<string>)\n" ++
      "\t./Main -r <regex> (<string>)\n" ++
      "Parameters:\n" ++
      "\tfilename: the name of the .fsm file to load and run\n" ++
      "\tregex: a regex to run\n" ++
      "\tstring: the string to be evaluated by the machine\n"
  in
    putStr message

runFSMOnString :: String -> String -> IO()
runFSMOnString filename string = do
  fsm <- loadFSM filename
  case fsm of
    Nothing ->
      print ("Invalid FSM specified in " ++ filename)
    Just actFSM ->
      print $ runFSM actFSM $ map FSM.Sym string

runRegexOnString :: String -> String -> IO()
runRegexOnString regexStr string = do
  case stringToRegex regexStr of
    Nothing -> print ("Invalid regex specified with " ++ regexStr)
    Just regex ->
      print $ runFSM (regexToFSM regex) $ map FSM.Sym string
