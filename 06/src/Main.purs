module Main where

import Prelude

import Data.Foldable (sum)
import Data.List.Types (List, NonEmptyList)
import Data.Set (fromFoldable, size, unions)
import Effect (Effect)
import Effect.Console (log, logShow)
import Input (input)
import Lib.Parser (emptyLine, oneConsecChar)
import Lib.Set (intersections)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (anyLetter)
import Text.Parsing.StringParser.Combinators (many1, sepBy)
    
parser :: Parser (List (List (NonEmptyList Char)))
parser = sepBy parserGroup emptyLine where
  parserGroup = sepBy (many1 anyLetter) (oneConsecChar '\n')

main :: Effect Unit
main = do
  let parsed = runParser parser input

  log "First task"
  logShow $ firstTask <$> parsed

  log "Second task"
  logShow $ secondTask <$> parsed

  where
    firstTask = map processGroup >>> sum where
      processGroup = map fromFoldable >>> unions >>> size

    secondTask = map processGroup >>> sum where
      processGroup = map fromFoldable >>> intersections >>> size
