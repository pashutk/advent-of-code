module Lib.List (splitAt, tails) where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), reverse, singleton, (:))

splitAt :: forall a. Int -> List a -> { init :: List a, rest :: List a }
splitAt at xs = reverseLists $ foldlWithIndex f {init: Nil, rest: Nil} xs where
  f i { init, rest } a | i < at = { init: a : init, rest           }
  f i { init, rest } a          = { init          , rest: a : rest }
  reverseLists { init, rest } = { init: reverse init, rest: reverse rest }

-- Not exported from Data.List
-- | Returns all final segments of the argument, longest first. For example,
-- |
-- | ```purescript
-- | tails (1 : 2 : 3 : Nil) == ((1 : 2 : 3 : Nil) : (2 : 3 : Nil) : (3 : Nil) : (Nil) : Nil)
-- | ```
-- | Running time: `O(n)`
tails :: forall a. List a -> List (List a)
tails Nil = singleton Nil
tails list@(Cons _ tl)= list : tails tl
