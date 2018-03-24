module ReadFSM where

import FSM

import qualified Data.Text as Txt
import qualified Data.Text.IO as Tio
import Data.Map.Strict as M
import Data.Set as S

type Transition = (Name, Symbol, Name)

loadFSM :: String -> IO FSM
loadFSM filename = do
  fileContents <- Tio.readFile filename
  return $ parseContents fileContents

parseContents :: Txt.Text -> FSM
parseContents contents =
  let
    (alphabetLine : startLine : acceptingLine : transitionLines) = Txt.lines contents
    alphabet = getAlphabet alphabetLine
    startName = getStartName startLine
    acceptingNames = getAcceptingNames acceptingLine
    transitions = readTransitions transitionLines
  in
    createFSM startName acceptingNames transitions alphabet

getAlphabet :: Txt.Text -> Alphabet
getAlphabet = S.fromList . Txt.words

getStartName :: Txt.Text -> Name
getStartName = id

getAcceptingNames :: Txt.Text -> [Name]
getAcceptingNames = Txt.words

readTransitions :: [Txt.Text] -> [Transition]
readTransitions [] = []
readTransitions (first : rest) =
  (getTransition first : readTransitions rest)

getTransition :: Txt.Text -> Transition
getTransition line =
 let
   [start, symbol, end] = Txt.words line
 in
   (start, symbol, end)

createFSM :: Name -> [Name] -> [Transition] -> Alphabet -> FSM
createFSM startName acceptingNames transitions alphabet =
  let
    initialStateMap = M.fromList $ Prelude.map makeAcceptingStateEntry
                      acceptingNames
    stateMap = interpretTransitions initialStateMap transitions
  in
    (M.findWithDefault (State (Txt.pack "start") M.empty False) startName stateMap,
      stateMap, alphabet)

makeAcceptingStateEntry :: Name -> (Name, State)
makeAcceptingStateEntry name =
  (name, State name M.empty True)

interpretTransitions :: (Map Name State) -> [Transition] -> (Map Name State)
interpretTransitions stateMap [] = stateMap
interpretTransitions stateMap ((origin, symbol, destination) : transitions) =
  let
    initialOriginState =
      findWithDefault (State origin M.empty False) origin stateMap
    originState =
      addTransitionToState (symbol, destination) initialOriginState
  in
    interpretTransitions (M.insert origin originState stateMap) transitions

addTransitionToState :: (Symbol, Name) -> State -> State
addTransitionToState (symbol, destination) (State name transitionMap accepting) =
  let
    destinations =
      (destination : findWithDefault [] symbol transitionMap)
    newTransitionMap =
      M.insert symbol destinations transitionMap
  in
    (State name newTransitionMap accepting)
