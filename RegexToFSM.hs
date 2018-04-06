module RegexToFSM where

import Data.Set as S (singleton, empty, fromList)
import Data.Map as M (empty, fromList)
import Data.Text (pack)
import FSM hiding (Sym)
import Regex

regexToFSM :: Regex -> FSM
regexToFSM (Sym Epsilon) =
  let
    startState = pack "0"
  in
    (singleton startState,
     S.empty,
     M.empty,
     startState,
     singleton startState)
regexToFSM (Sym symbol) =
  let
    startState = pack "0"
    symState = pack "1"
  in
    (S.fromList [startState, symState],
     singleton symbol,
     M.fromList [((startState, symbol), singleton symState)],
     startState,
     singleton symState)
