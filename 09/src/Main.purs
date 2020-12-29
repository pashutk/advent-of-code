module Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (maximum, minimum, sum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), any, filter, reverse, snoc, (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Input (input)
import Lib.Parser (number)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (char)
import Text.Parsing.StringParser.Combinators (sepBy)

parser :: Parser (List Number)
parser = sepBy number (char '\n')

main :: Effect Unit
main = do
  let
    preambleSize = 25
    parsed = runParser parser input
    numbers = case parsed of
      Left _ -> Nil
      Right a -> a
    {h, t} = foldlWithIndex f {h: Nil, t: Nil} numbers where
      f i {h, t} a | i < preambleSize = {h: a : h, t}
      f i {h, t} a = {h, t: a : t}
    preamble = reverse h
    body = reverse t

    findFirstInvalid = f preamble body where
      check p bs = any (\a -> any (\b -> a /= b && a + b == p) possibleRight) possibleLeft where
        possibleLeft = filter (_ < p / 2.0) bs
        possibleRight = filter (_ < p) bs
      f _ Nil = Left "Not found"
      f Nil _ = Left "Empty preamble"
      f p@(ph: pt) (bh : bt) = case check bh p of
        true -> f (snoc pt bh) bt
        false -> Right bh

    firstInvalid = case findFirstInvalid of
      Left _ -> -1.0
      Right a -> a
      
    secondResult = f numbers where
      c :: List Number -> List Number -> Maybe (List Number)
      c Nil _ = Nothing
      c (ch : ct) Nil = c ct (ch: Nil)
      c _ r | sum r == firstInvalid = Just r
      c _ r | sum r > firstInvalid = Nothing
      c (ch : ct) r = c ct (ch : r)

      f Nil = Nil
      f n@(nh : nt) = case c n Nil of
        Nothing -> f nt
        Just res -> res

  log "First task:"
  logShow firstInvalid

  log "Second task:"
  logShow do
    mi <- minimum secondResult
    ma <- maximum secondResult
    pure $ mi + ma

