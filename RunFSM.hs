module RunFSM where

import Data.Map.Strict as M (findWithDefault)
import FSM

runFSM :: FSM -> [Symbol] -> Bool
runFSM fsm@(_, _, _, start, _) string =
  runStep fsm string start

runStep :: FSM -> [Symbol] -> State -> Bool
runStep (_, _, _, _, accepting) [] state = elem state accepting
runStep fsm@(_, _, stateMap, _, _) (next : rest) state =
  let
    children = findWithDefault [] (state, next) stateMap
  in
    any (\ child -> runStep fsm rest child) children
