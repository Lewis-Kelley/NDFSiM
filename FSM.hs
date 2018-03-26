module FSM where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)

type State = Text
type Symbol = Text
type Alphabet = S.Set Symbol

type FSM = ([State], Alphabet, M.Map (State, Symbol) [State], State, [State])

isDFSM :: FSM -> Bool
isDFSM (states, alphabet, delta, _, _) =
  all (\ state ->
         (all (\ symbol ->
                 (length $ M.lookup (state, symbol) delta) == 1) alphabet)) states
