module Lib.Set
  ( intersections
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Set (Set, empty, intersection)

intersections :: forall a. Ord a => List (Set a) -> Set a
intersections Nil = empty
intersections (Cons h Nil) = h
intersections (Cons h t) = foldl intersection h t
