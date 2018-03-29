module RunFSM where

import Data.Map.Strict as M (findWithDefault)
import Data.Set as S
import FSM

runFSM :: FSM -> [Symbol] -> Bool
runFSM fsm@(_, _, _, start, _) string =
  runStep fsm string start

runStep :: FSM -> [Symbol] -> State -> Bool
runStep (_, _, _, _, accepting) [] state = elem state accepting
runStep fsm@(_, _, stateMap, _, _) (next : rest) state =
  let
    children = findWithDefault [] (state, next) stateMap
    epsChildren = S.toList $ epsilonClosure fsm $ S.singleton state
  in
    any (\ child -> runStep fsm rest child) children
    || any (\ epsChild -> runStep fsm rest epsChild) epsChildren

epsilonClosure :: FSM -> Set State -> Set State
epsilonClosure fsm@(_, _, stateMap, _, _) states =
  let
    nested = S.map (\ state ->
                           findWithDefault [] (state, Epsilon) stateMap) states
    currClosure = S.foldr (\ list rest ->
                              S.union (S.fromList list) rest) S.empty nested
  in
    if currClosure == states
    then
      currClosure
    else
      epsilonClosure fsm currClosure
