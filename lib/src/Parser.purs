module Lib.Parser 
  ( digit
  , digitInt
  , emptyLine
  , int
  , manyAs1
  , oneConsecChar
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (length)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (pow)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.CodeUnits (anyDigit, char, satisfy, string)
import Text.Parsing.StringParser.Combinators (lookAhead, many, many1)

digit :: Parser Char
digit = anyDigit

digitInt :: Parser Int
digitInt =  string "0" $> 0
        <|> string "1" $> 1
        <|> string "2" $> 2
        <|> string "3" $> 3
        <|> string "4" $> 4
        <|> string "5" $> 5
        <|> string "6" $> 6
        <|> string "7" $> 7
        <|> string "8" $> 8
        <|> string "9" $> 9

-- Works only for small ints when radix is 10 (~ <2^31)
int :: Int -> Parser Int
int radix = f <*> length <$> many1 digitInt where
    f ns l = foldrWithIndex f' 0 ns where
        f' i a b = pow radix (l - i - 1) * a + b

manyAs1 :: Char -> Parser Char
manyAs1 c = many (char c) $> c

-- Parses to Right only if there is no consecutive char occurrences
oneConsecChar :: Char -> Parser Char
oneConsecChar c = try do
  a <- char c
  b <- lookAhead $ satisfy (_ /= c)
  pure a

emptyLine :: Parser Char
emptyLine = do
  _ <- char '\n'
  a <- char '\n'
  pure a
