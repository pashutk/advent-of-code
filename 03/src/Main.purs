module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fromFoldable, length, (!!))
import Data.Foldable (product)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Input (input)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (char)
import Text.Parsing.StringParser.Combinators (many, sepEndBy)

data MapCell = OpenSquare | TreeSquare

instance showMapCell :: Show MapCell where
  show OpenSquare = "."
  show TreeSquare = "🌳"

parserMapCell :: Parser MapCell
parserMapCell = char '.' $> OpenSquare <|> char '#' $> TreeSquare

type Row = List MapCell

parserRow :: Parser Row
parserRow = many parserMapCell

type RowA = Array MapCell

parserRowA :: Parser RowA
parserRowA = fromFoldable <$> parserRow

type Map = List Row

parserMap :: Parser Map
parserMap = sepEndBy parserRow $ char '\n'

type MapA = List RowA

parserMapA :: Parser MapA
parserMapA = sepEndBy parserRowA $ char '\n'

inc :: Int -> Int
inc = add 1

dec :: Int -> Int
dec a = sub a 1

countTrees :: Int -> Int -> MapA -> Int
countTrees xStep yStep map = check map 0 0 0 where
  oneIfTreeOnIndex :: RowA -> Int -> Int
  oneIfTreeOnIndex r' i = f $ r' !! i where
    f (Just TreeSquare) = 1
    f _ = 0

  check :: MapA -> Int -> Int -> Int -> Int
  check Nil row column result = result
  check (Cons h t) 0 column result = check t row' column' result' where
    row' = dec yStep
    column' = mod (column + xStep) (length h)
    result' = result + oneIfTreeOnIndex h column
  check (Cons _ t) row c r = check t (dec row) c r

main :: Effect Unit
main = do
  log "First task result:"
  log $ show $ countTrees 3 1 <$> runParser parserMapA input
  
  log "Second task result:"
  log $ show $ f <$> runParser parserMapA input

  pure unit where
    f m = product $ toNumber <$> do
      a <- [countTrees 1 1, countTrees 3 1, countTrees 5 1, countTrees 7 1, countTrees 1 2]
      pure (a m)
      