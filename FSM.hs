module FSM where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.Set

type State = Text
data Symbol = Sym Char | Epsilon
  deriving (Eq, Ord, Show)

type Alphabet = S.Set Symbol
type TransitionMap = M.Map (State, Symbol) (Set State)

type FSM = (Set State, Alphabet, TransitionMap, State, Set State)

isDFSM :: FSM -> Bool
isDFSM (states, alphabet, delta, _, _) =
  all (\ state ->
         (all (\ symbol ->
                 (length $ M.lookup (state, symbol) delta) == 1) alphabet)) states
