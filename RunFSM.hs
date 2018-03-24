module RunFSM where

import Data.Map.Strict as M (findWithDefault, lookup)
import Data.Maybe
import FSM

runFSM :: FSM -> [Symbol] -> Bool
runFSM fsm@(start, _, _) string =
  runStep fsm string start

runStep :: FSM -> [Symbol] -> State -> Bool
runStep _ [] (State _ _ accepting) = accepting
runStep fsm@(_, stateMap, _) (next : rest) (State _ transitions _) =
  let
    nextNames = M.findWithDefault [] next transitions
    nextStages = map (\ name -> M.lookup name stateMap) nextNames
    results = map (runStep fsm rest) $ catMaybes nextStages
  in
    foldr (||) False results
