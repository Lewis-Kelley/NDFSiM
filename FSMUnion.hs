module FSMUnion where

import Data.Text (Text, cons, pack)
import Data.Set as S (Set, union, map, insert, fromList)
import Data.Map.Strict as M (union, insert, empty, foldrWithKey)
import FSM

unionFSMs :: FSM -> FSM -> FSM
unionFSMs (leftStates, leftAlphabet, leftTransitions, leftStart, leftAccepting)
  (rightStates, rightAlphabet, rightTransitions, rightStart, rightAccepting) =
  let
    markLeft = cons '0'
    markRight = cons '1'
    fullStates = unionStates markLeft markRight leftStates rightStates
    fullAlphabet = unionAlphabets leftAlphabet rightAlphabet
    fullTransitions
      = unionTransitions markLeft markRight leftStart rightStart leftTransitions rightTransitions
    fullAccepting = unionAcceptingStates markLeft markRight leftAccepting rightAccepting
  in
    (fullStates, fullAlphabet, fullTransitions, makeStartState, fullAccepting)

unionStates :: (Text -> Text) -> (Text -> Text) -> Set State -> Set State
            -> Set State
unionStates markLeft markRight leftStates rightStates =
  let
    startState = makeStartState
    newLeftStates = S.map markLeft leftStates
    newRightStates = S.map markRight rightStates
    mergedStates = S.union newLeftStates newRightStates
    fullStates = S.insert startState mergedStates
  in
    fullStates

unionAlphabets :: Alphabet -> Alphabet -> Alphabet
unionAlphabets = S.union

unionTransitions :: (Text -> Text) -> (Text -> Text) -> State -> State
                 -> TransitionMap -> TransitionMap -> TransitionMap
unionTransitions markLeft markRight leftStartState rightStartState
  leftTransitions rightTransitions =
  let
    startState = makeStartState
    newLeftTransitions = markTransitions leftTransitions markLeft
    newRightTransitions = markTransitions rightTransitions markRight
    mergedTransitions = M.union newLeftTransitions newRightTransitions
    fullTransitions
      = M.insert (startState, Epsilon) (S.fromList [(markLeft leftStartState),
                                                    (markRight rightStartState)]) mergedTransitions
  in
    fullTransitions

markTransitions :: TransitionMap -> (Text -> Text) -> TransitionMap
markTransitions transitions mark =
  foldrWithKey (\ (start, sym) end rest ->
                  M.insert (mark start, sym) (S.map mark end) rest) M.empty transitions

unionAcceptingStates :: (Text -> Text) -> (Text -> Text) -> Set State -> Set State -> Set State
unionAcceptingStates markLeft markRight leftAccepting rightAccepting =
  S.union (S.map markLeft leftAccepting) (S.map markRight rightAccepting)

makeStartState :: State
makeStartState = pack "0"
