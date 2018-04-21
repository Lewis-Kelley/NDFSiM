module FSMStar where

import Data.Map.Strict as M (insert, insertWith)
import Data.Set as S (Set, foldr, map, singleton, union)
import Data.Text (Text, cons)

import FSM
import FSMConstruction

starFSM :: FSM -> FSM
starFSM (states, alphabet, transitions, start, accepting) =
  let
    mark = cons '0'
    fullStates = starStates mark states
    fullTransitions = starTransitions mark start accepting transitions
    newStart = makeStartState
    fullAccepting = starAcceptingStates mark accepting
  in
    (fullStates, alphabet, fullTransitions, newStart, fullAccepting)

starStates :: (Text -> Text) -> Set State -> Set State
starStates = markAllAndAddStart

starAcceptingStates :: (Text -> Text) -> Set State -> Set State
starAcceptingStates = markAllAndAddStart

markAllAndAddStart :: (Text -> Text) -> Set State -> Set State
markAllAndAddStart mark states =
  S.union (singleton makeStartState) $ S.map mark states

starTransitions :: (Text -> Text) -> State -> Set State -> TransitionMap
                -> TransitionMap
starTransitions mark oldStart accepting transitions =
  let
    startState = makeStartState
    markedAccepting = S.map mark accepting
    markedTransitions = markTransitions transitions mark
    loopedTransitions
      = addLoopingTransitions markedAccepting startState markedTransitions
    fullTransitions = M.insert (startState, Epsilon) (singleton $ mark oldStart)
                      loopedTransitions
  in
    fullTransitions

addLoopingTransitions :: Set State -> State -> TransitionMap -> TransitionMap
addLoopingTransitions accepting start transitions =
  S.foldr (\ input folded ->
             addLoopingTransition input start folded) transitions accepting

addLoopingTransition :: State -> State -> TransitionMap -> TransitionMap
addLoopingTransition start end transitions =
  insertWith (\ oldValue newValue ->
                S.union oldValue newValue) (start, Epsilon) (singleton end) transitions
