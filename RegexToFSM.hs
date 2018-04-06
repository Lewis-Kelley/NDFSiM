module RegexToFSM where

import Data.Map.Strict as M (fromList, empty)
import Data.Set as S (fromList, empty, singleton)
import Data.Text (pack)

import FSM hiding (Sym)
import Regex
import FSMUnion

regexToFSM :: Regex -> FSM
regexToFSM (Sym Epsilon) =
  let
    startState = makeStartState
  in
    (singleton startState,
     S.empty,
     M.empty,
     startState,
     singleton startState)
regexToFSM (Sym symbol) =
  let
    startState = makeStartState
    symState = pack "1"
  in
    (S.fromList [startState, symState],
     singleton symbol,
     M.fromList [((startState, symbol), singleton symState)],
     startState,
     singleton symState)
regexToFSM (Union leftRegex rightRegex) =
  let
    leftFSM = regexToFSM leftRegex
    rightFSM = regexToFSM rightRegex
  in
    unionFSMs leftFSM rightFSM
