module Regex where

import FSM (Symbol(Epsilon, Sym))

data Regex = Sym Symbol
           | Union Regex Regex
           | Star Regex
           | Plus Regex
           | Concat Regex Regex
  deriving (Eq, Ord, Show)

-- Splits up the Regex at the Unions and passes it off to parseRegex
stringToRegex :: String -> Maybe Regex
stringToRegex str = do
  (beforeUnion, afterUnion) <- splitAtUnion str
  case afterUnion of
    "" -> parseRegex beforeUnion
    _ -> do
      headRegex <- parseRegex beforeUnion
      tailRegex <- stringToRegex afterUnion
      Just $ Union headRegex tailRegex

-- Doesn't handle unions
parseRegex :: String -> Maybe Regex
parseRegex "" = Just $ Regex.Sym Epsilon
parseRegex ('*' : _) = Nothing
parseRegex ('+' : _) = Nothing
parseRegex ('(' : afterOpen) = do
  let (nested, afterParen) = takeUntilCloseParen afterOpen
  innerRegex <- stringToRegex nested
  case afterParen of
    ('*' : rest) -> do
      tailRegex <- parseRegex rest
      Just $ Concat (Star innerRegex) tailRegex
    ('+' : rest) -> do
      tailRegex <- parseRegex rest
      Just $ Concat (Plus innerRegex) tailRegex
    rest -> do
      tailRegex <- parseRegex rest
      Just $ Concat innerRegex tailRegex
parseRegex [firstChar] =
  Just $ makeRegSym firstChar
parseRegex (firstChar : '*' : rest) = do
  tailRegex <- parseRegex rest
  Just $ Concat (Star $ makeRegSym firstChar) tailRegex
parseRegex (firstChar : '+' : rest) = do
  tailRegex <- parseRegex rest
  Just $ Concat (Plus $ makeRegSym firstChar) tailRegex
parseRegex (firstChar : rest) = do
  tailRegex <- parseRegex rest
  Just $ Concat (makeRegSym firstChar) tailRegex

makeRegSym :: Char -> Regex
makeRegSym '_' = Regex.Sym $ Epsilon
makeRegSym char = Regex.Sym $ FSM.Sym char

splitAtUnion :: String -> Maybe (String, String)
splitAtUnion "" = Just ("", "")
splitAtUnion ('U' : rest)
  | rest == "" = Nothing
  | otherwise = Just ("", rest)
splitAtUnion str@('(' : _) = do
  (nested, rest) <- takeOuterParenPair str
  (beforeUnion, afterUnion) <- splitAtUnion rest
  Just ("(" ++ nested ++ ")" ++ beforeUnion, afterUnion)
splitAtUnion (firstChar : rest) = do
  (beforeUnion, afterUnion) <- splitAtUnion rest
  Just (firstChar : beforeUnion, afterUnion)

takeOuterParenPair :: String -> Maybe (String, String)
takeOuterParenPair "" = Nothing
takeOuterParenPair ('(' : rest) = Just $ takeUntilCloseParen rest
takeOuterParenPair (_ : rest) = takeOuterParenPair rest

takeUntilCloseParen :: String -> (String, String)
takeUntilCloseParen ('(' : rest) =
  let
    (nested, afterNested) = takeUntilCloseParen rest
    (beforeClose, afterClose) = takeUntilCloseParen afterNested
  in
    ("(" ++ nested ++ ")" ++ beforeClose, afterClose)
takeUntilCloseParen (')' : rest) = ("", rest)
takeUntilCloseParen (firstChar : rest) =
  let
    (beforeClose, afterClose) = takeUntilCloseParen rest
  in
    ((firstChar : beforeClose), afterClose)
takeUntilCloseParen "" = ("", "")
