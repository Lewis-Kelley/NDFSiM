module ReadFSM where

import FSM

import Data.Text as Txt
import Data.Text.IO as Tio
import Data.Map.Strict as M hiding (toList)
import Data.Set as S
import Prelude as P

type Transition = (State, Symbol, State)

loadFSM :: String -> IO (Maybe FSM)
loadFSM filename = do
  fileContents <- Tio.readFile filename
  return $ parseContents fileContents

parseContents :: Txt.Text -> Maybe FSM
parseContents contents = do
  (alphabetLine, startLine, acceptingLine, transitionLines) <- splitFile contents
  alphabet <- getAlphabet alphabetLine
  start <- getStart startLine
  transitions <- readTransitions transitionLines
  let states = getStates transitions
  let accepting = getAccepting acceptingLine
  Just (states, alphabet, transitions, start, accepting)

splitFile :: Txt.Text -> Maybe (Txt.Text, Txt.Text, Txt.Text, [Txt.Text])
splitFile contents =
  case Txt.lines contents of
    (alphabetLine : startLine : acceptingLine : transitionLines) ->
      Just (alphabetLine, startLine, acceptingLine, transitionLines)
    _ -> Nothing

getAlphabet :: Txt.Text -> Maybe Alphabet
getAlphabet line =
  S.foldr parseAlphabet (Just S.empty)
  (S.fromList $ P.map Txt.unpack $ Txt.words line)

parseAlphabet :: [Char] -> Maybe Alphabet -> Maybe Alphabet
parseAlphabet _ Nothing = Nothing
parseAlphabet [sym] (Just alphabet) = Just $ S.insert (Sym sym) alphabet
parseAlphabet _ _ = Nothing

getStart :: Txt.Text -> Maybe State
getStart str =
  case Txt.find (' ' ==) str of
    Nothing -> Just str
    Just _ -> Nothing

getAccepting :: Txt.Text -> Set State
getAccepting = S.fromList . Txt.words

readTransitions :: [Txt.Text] -> Maybe TransitionMap
readTransitions [] = Just M.empty
readTransitions (first : rest) = do
  transitions <- readTransitions rest
  (start, symbol, end) <- getTransition first
  let children = M.findWithDefault S.empty (start, symbol) transitions
  Just $ M.insert (start, symbol) (S.insert end children) transitions

getTransition :: Txt.Text -> Maybe Transition
getTransition line =
  case Txt.words line of
    [start, symbolText, end] ->
      case Txt.unpack symbolText of
        "_" -> Just (start, Epsilon, end)
        [symbol] -> Just (start, Sym symbol, end)
        _ ->  Nothing
    _ -> Nothing

getStates :: TransitionMap -> Set State
getStates = S.map fst . keysSet
