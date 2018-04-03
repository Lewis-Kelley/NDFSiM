module ReadFSM where

import FSM

import Data.Text as Txt
import Data.Text.IO as Tio
import Data.Map.Strict as M hiding (toList)
import Data.Set as S

type Transition = (State, Symbol, State)

loadFSM :: String -> IO FSM
loadFSM filename = do
  fileContents <- Tio.readFile filename
  return $ parseContents fileContents

parseContents :: Txt.Text -> FSM
parseContents contents =
  let
    (alphabetLine : startLine : acceptingLine : transitionLines) = Txt.lines contents
    alphabet = getAlphabet alphabetLine
    start = getStart startLine
    accepting = getAccepting acceptingLine
    transitions = readTransitions transitionLines
    states = getStates transitions
  in
    (states, alphabet, transitions, start, accepting)

getAlphabet :: Txt.Text -> Alphabet
getAlphabet = S.map (\ char -> Sym char) . S.fromList . Txt.words

getStart :: Txt.Text -> State
getStart = id

getAccepting :: Txt.Text -> Set State
getAccepting = S.fromList . Txt.words

readTransitions :: [Txt.Text] -> TransitionMap
readTransitions [] = M.empty
readTransitions (first : rest) =
  let
    transitions = readTransitions rest
    (start, symbol, end) = getTransition first
    children = M.findWithDefault S.empty (start, symbol) transitions
  in
    M.insert (start, symbol) (S.insert end children) transitions

getTransition :: Txt.Text -> Transition
getTransition line =
 let
   [start, symbol, end] = Txt.words line
 in
   if symbol == Txt.pack "_"
   then
     (start, Epsilon, end)
   else
     (start, Sym symbol, end)

getStates :: TransitionMap -> Set State
getStates = S.map fst . keysSet
