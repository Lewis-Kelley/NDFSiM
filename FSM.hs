module FSM where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)

data State = State Name (M.Map Symbol [Name]) Bool
  deriving (Eq, Ord, Show)

type Name = Text

type FSM = (State, [State], Alphabet)

type Symbol = Text
type Alphabet = S.Set Symbol

isDFSM :: FSM -> Bool
isDFSM (_, states, alphabet) =
  all (\ state@(State _ transitions _) ->
         (M.keysSet transitions) == alphabet
         && (all (\ childrenSet ->
                     length childrenSet == 1)
              $ getChildrenSets alphabet state)) states

getChildrenSets :: Alphabet -> State -> S.Set [Name]
getChildrenSets alphabet (State _ transitions _) =
  S.map (\ letter ->
            case M.lookup letter transitions of
              Nothing -> []
              Just states -> states) alphabet
