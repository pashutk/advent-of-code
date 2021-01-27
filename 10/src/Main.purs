module Main where

import Prelude

import Data.Either (fromRight)
import Data.Foldable (sum)
import Data.List (List(..), drop, filter, length, reverse, sort, take, zipWith, (:))
import Data.List.Partial (last)
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Input (input)
import Lib.Parser (int)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (char)
import Text.Parsing.StringParser.Combinators (sepBy)

parser :: Parser (List Int)
parser = sepBy (int 10) $ char '\n'

count :: forall a. Eq a => a -> List a -> Int
count n = length <<< filter (_ /= n)

sliding :: forall a. List a -> List (List a)
sliding Nil = Nil
sliding xs = take 4 xs : sliding (drop 1 xs)

unsafeLookup :: forall k v. Ord k => k -> Map k v -> v
unsafeLookup k m = unsafePartial fromJust $ lookup k m

main :: Effect Unit
main = do
  let
    parsed = runParser parser input
    justParsed = unsafePartial fromRight parsed
    sortedParsed = sort justParsed
    ratings = 0 : sortedParsed
    diffs = 3 : zipWith (-) sortedParsed ratings
    firstResult = (count 1 diffs) * (count 3 diffs)

    reversedRatings = reverse ratings
    goal = unsafePartial last ratings

    edges :: Map Int (List Int)
    edges = fromFoldable $ f <$> sliding ratings where
      f Nil = Tuple 0 Nil
      f (h : t) = Tuple h $ filter (_ <= h + 3) t

    createCache :: List Int -> Map Int Number
    createCache = fromFoldable <<< map (flip Tuple 0.0)

    secondResult = unsafePartial fromJust $ lookup 0 $ fulfillCache reversedRatings where
      fulfillCache :: List Int -> Map Int Number
      fulfillCache rs = f rs $ createCache rs where
        f :: List Int -> Map Int Number -> Map Int Number
        f Nil cache = cache
        f (h : t) cache = f t $ insert h (f'' cache h) cache where
          f'' :: Map Int Number -> Int -> Number
          f'' m x | x == goal = 1.0
          f'' m x = sum $ flip unsafeLookup m <$> unsafeLookup x edges

  logShow firstResult
  logShow secondResult

