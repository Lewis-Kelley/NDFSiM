module RunFSM where

import Data.Map.Strict as M (findWithDefault)
import Data.Set as S
import FSM

runFSM :: FSM -> [Symbol] -> Bool
runFSM fsm@(_, _, _, start, _) string =
  runStep fsm string start

runStep :: FSM -> [Symbol] -> State -> Bool
runStep (_, _, _, _, accepting) [] state = elem state accepting
runStep fsm@(_, _, transitionMap, _, _) (next : rest) state =
  let
    children = findWithDefault empty (state, next) transitionMap
    epsChildren = S.toList $ epsilonClosure fsm $ S.singleton state
  in
    any (\ child -> runStep fsm rest child) children
    || any (\ epsChild -> runStep fsm rest epsChild) epsChildren

epsilonClosure :: FSM -> Set State -> Set State
epsilonClosure fsm@(_, _, transitionMap, _, _) states =
  let
    nested = S.map (\ state ->
                       findWithDefault empty (state, Epsilon) transitionMap) states
    currClosure = S.foldr (\ inner rest ->
                              S.union inner rest) empty nested
  in
    if currClosure == states
    then
      currClosure
    else
      epsilonClosure fsm currClosure
