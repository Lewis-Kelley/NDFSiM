module FSMConstruction where

import Data.Map.Strict as M (empty, insert, foldrWithKey)
import Data.Set as S (map)
import Data.Text (Text)

import FSM

markTransitions :: TransitionMap -> (Text -> Text) -> TransitionMap
markTransitions transitions mark =
  foldrWithKey (\ (start, sym) end rest ->
                  M.insert (mark start, sym) (S.map mark end) rest) M.empty transitions
