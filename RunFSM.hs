module RunFSM where

import Data.Map.Strict as M (findWithDefault)
import Data.Set as S
import FSM

type Configuration = (State, [Symbol])

runFSM :: FSM -> [Symbol] -> Bool
runFSM fsm@(_, _, _, start, _) string =
  runStep fsm (start, string) empty

runStep :: FSM -> Configuration -> Set Configuration -> Bool
runStep fsm@(_, _, _, _, accepting) conf@(state, []) history
  | elem conf history = False
  | otherwise =
    let
      epsChildren = S.toList $ epsilonClosure fsm $ S.singleton state
      newHistory = union history $ singleton conf
    in
      elem state accepting
      || any (\ epsChild -> runStep fsm (epsChild, []) newHistory) epsChildren
runStep fsm@(_, _, transitionMap, _, _) conf@(state, full@(next : rest)) history
  | elem conf history = False
  | otherwise =
    let
      children = findWithDefault empty (state, next) transitionMap
      epsChildren = S.toList $ epsilonClosure fsm $ S.singleton state
      newHistory = union history $ singleton conf
    in
      any (\ child -> runStep fsm (child, rest) newHistory) children
      || any (\ epsChild -> runStep fsm (epsChild, full) newHistory) epsChildren

epsilonClosure :: FSM -> Set State -> Set State
epsilonClosure fsm@(_, _, transitionMap, _, _) states =
  let
    nested = S.map (\ state ->
                       findWithDefault empty (state, Epsilon) transitionMap) states
    currClosure = S.foldr (\ inner rest ->
                              S.union inner rest) states nested
  in
    if currClosure == states
    then
      currClosure
    else
      epsilonClosure fsm currClosure
