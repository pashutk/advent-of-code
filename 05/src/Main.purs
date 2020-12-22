module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldl, replicate)
import Data.Int.Bits (or, shl)
import Data.List (List(..), sort)
import Data.Traversable (maximum, sequence)
import Effect (Effect)
import Effect.Console (log)
import Input (input)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (char)
import Text.Parsing.StringParser.Combinators (sepBy)

parserSeat :: Parser { row :: Array Char, column :: Array Char }
parserSeat = do
  r <- sequence $ replicate 7 $ char 'F' <|> char 'B'
  c <- sequence $ replicate 3 $ char 'L' <|> char 'R'
  pure $ { row: r, column: c }

binArrToInt :: Array Int -> Int
binArrToInt = foldl (flip shl 1 >>> or) 0

toBin :: Char -> Char -> Array Char -> Array Int
toBin zero one = map f' where
  f' a | a == zero = 0
  f' a | a == one = 1
  f' a | otherwise = 0

main :: Effect Unit
main = do
  log "First task"
  log $ show $ firstTask <$> f <$> runParser (sepBy parserSeat (char '\n')) input
  log "Second task"
  log $ show $ secondTask <$> f <$> runParser (sepBy parserSeat (char '\n')) input
  where
    f   = map (\{row, column} -> {row: toBin 'F' 'B' row, column: toBin 'L' 'R' column})
      >>> map (\{row, column} -> { row: binArrToInt row, column: binArrToInt column })
      >>> map (\{row, column} -> row * 8 + column)

    firstTask = maximum

    secondTask = sort >>> f' where
      f' (Cons h (Cons h1 t)) | h + 1 /= h1 = h + 1
      f' (Cons h t) = f' t
      f' Nil = -1
