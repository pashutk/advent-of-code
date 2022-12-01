module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Either (fromRight)
import Data.Foldable (class Foldable, elem, findMap, length, maximum, minimum, sum)
import Data.List (List(..), filter, (:))
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Console (log, logShow)
import Input (input)
import Lib.List (splitAt, tails)
import Lib.Parser (number)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (char)
import Text.Parsing.StringParser.Combinators (sepBy)

parser :: Parser (List Number)
parser = sepBy number (char '\n')

containTerms :: List Number -> Number -> Boolean
containTerms xs x = elem x do
  a <- filter (_ < x / 2.0) xs
  b <- xs
  guard $ a /= b
  pure $ a + b

findInvalid :: Int -> List Number -> Maybe Number
findInvalid _ Nil = Nothing
findInvalid preambleSize xs | length xs <= preambleSize  = Nothing
findInvalid preambleSize xs@(_ : xstail) = case splitAt preambleSize xs of
  { init, rest: (current : _) } | containTerms init current -> findInvalid preambleSize xstail
  {       rest: (current : _) } | otherwise                 -> Just current
  _ -> Nothing

extremumSum :: forall f a. Semiring a => Ord a => Foldable f => f a -> Maybe a
extremumSum xs = do
  minima <- minimum xs
  maxima <- maximum xs
  pure $ minima + maxima

findConsecSum :: forall a. Eq a => Semiring a => Ord a => a -> List a -> Maybe (List a)
findConsecSum x xs = findMap (f Nil) (tails xs) where
  f :: List a -> List a -> Maybe (List a)
  f _ Nil                = Nothing
  f res _ | sum res == x = Just res
  f res _ | sum res > x  = Nothing
  f res (ch : ct)        = f (ch : res) ct

main :: Effect Unit
main = do
  let
    numbers = runParser parser input
    justNumbers = unsafePartial fromRight numbers

    firstInvalid = findInvalid 25 justNumbers
    justFirstInvalid = unsafePartial fromJust firstInvalid

    secondResult = findConsecSum justFirstInvalid justNumbers >>= extremumSum 
    justSecondResult = unsafePartial fromJust secondResult

  log "First task:"
  logShow justFirstInvalid

  log "Second task:"
  logShow justSecondResult
