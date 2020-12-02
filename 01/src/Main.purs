module Main where

import Prelude

import Data.Array as A
import Data.Foldable (product)
import Data.Int (toNumber)
import Data.List (List(..), concatMap, filter, fromFoldable, singleton)
import Data.Number (fromString)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Input (input)

splitLines :: String -> Array String
splitLines = split (Pattern "\n")

getArrayOfNumbers :: String -> Array Number
getArrayOfNumbers = splitLines >>> A.mapMaybe fromString

getFirstSumMembers :: Number -> Int -> List Number -> List Number
getFirstSumMembers _ 1 = identity
getFirstSumMembers sum count = filter (_ <= firstElemMax) where
  firstElemMax = sum / toNumber count

filterExact :: Number -> List Number -> List Number
filterExact a = filter (_ == a)

getSumMembers :: Number -> Int -> List Number -> List (List Number)
getSumMembers total count numbers = f' total count numbers Nil where
    f' _ 0 _ _ = Nil
    f' s 1 n Nil = filterExact s >>> map singleton $ n
    f' s 1 n l = filterExact s >>> map (flip Cons l) $ n
    f' s c n l = concatMap f $ getFirstSumMembers s c n where
      f a = f' (s - a) (c - 1) (filter (_ /= a) n) (Cons a l)

main :: Effect Unit
main = (getArrayOfNumbers >>> fromFoldable >>> work >>> log) input where
  work ns = work' where
    total = 2020.0
    firstTaskResult = map product $ getSumMembers total 2 ns
    secondTaskResult = map product $ getSumMembers total 3 ns
    work' = (show firstTaskResult) <> "\n" <> (show secondTaskResult)
