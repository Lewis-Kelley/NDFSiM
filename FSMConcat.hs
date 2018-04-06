module FSMConcat where

import Data.Set as S (Set, union, map, foldr, singleton)
import Data.Map.Strict as M (union, insertWith)
import Data.Text (Text, cons)

import FSM
import FSMConstruction

concatFSMs :: FSM -> FSM -> FSM
concatFSMs (leftStates, leftAlphabet, leftTransitions, leftStart, leftAccepting)
  (rightStates, rightAlphabet, rightTransitions, rightStart, rightAccepting) =
  let
    markLeft = cons '0'
    markRight = cons '1'
    fullStates = concatStates markLeft markRight leftStates rightStates
    fullAlphabet = concatAlphabet leftAlphabet rightAlphabet
    fullTransitions
      = concatTransitions markLeft markRight
        leftAccepting rightStart
        leftTransitions rightTransitions
    start = markLeft leftStart
    accepting = markAccepting markRight rightAccepting
  in
    (fullStates, fullAlphabet, fullTransitions, start, accepting)

concatStates :: (Text -> Text) -> (Text -> Text) -> Set State -> Set State -> Set State
concatStates markLeft markRight leftStates rightStates =
  S.union (S.map markLeft leftStates) (S.map markRight rightStates)

concatAlphabet :: Alphabet -> Alphabet -> Alphabet
concatAlphabet = S.union

concatTransitions :: (Text -> Text) -> (Text -> Text)
                  -> Set State -> State
                  -> TransitionMap -> TransitionMap
                  -> TransitionMap
concatTransitions markLeft markRight leftAccepting rightStart leftTransitions rightTransitions =
  let
    newLeftTransitions = markTransitions leftTransitions markLeft
    newRightTransitions = markTransitions rightTransitions markRight
    mergedTransitions = M.union newLeftTransitions newRightTransitions
    fullTransitions = addAcceptedTransitions
                      (markAccepting markLeft leftAccepting) (markRight rightStart)
                      mergedTransitions
  in
    fullTransitions

addAcceptedTransitions :: Set State -> State -> TransitionMap -> TransitionMap
addAcceptedTransitions outputs input transitions =
  S.foldr (\ output newTransitions ->
             addAcceptedTransition output input newTransitions) transitions outputs

addAcceptedTransition :: State -> State -> TransitionMap -> TransitionMap
addAcceptedTransition start end transitions =
  insertWith (\ newValue oldValue ->
                 S.union newValue oldValue)
  (start, Epsilon) (singleton end) transitions

markAccepting :: (Text -> Text) -> Set State -> Set State
markAccepting = S.map
