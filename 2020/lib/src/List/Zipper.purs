module Lib.List.Zipper (
    Zipper(..)
  , fromList
  , forward
  , backward
  , jump
  , current
  , toList
) where

import Prelude

import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))

data Zipper a = Empty | Zipper (List a) a (List a)

fromList :: forall a. List a -> Zipper a
fromList Nil = Empty
fromList (h : t) = Zipper Nil h t

toList :: forall a. Zipper a -> List a
toList Empty = Nil
toList (Zipper Nil h t) = h : t
toList (Zipper b c f) = reverse b <> (c : f)

forward :: forall a. Zipper a -> Maybe (Zipper a)
forward Empty = Nothing
forward (Zipper _ _ Nil) = Nothing
forward (Zipper b c (h : t)) = Just $ Zipper (c : b) h t

backward :: forall a. Zipper a -> Maybe (Zipper a)
backward Empty = Nothing
backward (Zipper Nil _ _) = Nothing
backward (Zipper (h : t) c f) = Just $ Zipper t h (c : f)

jump :: forall a. Int -> Zipper a -> Maybe (Zipper a)
jump i z | i > 0 = forward z >>= jump (i - 1)
jump i z | i < 0 = backward z >>= jump (i + 1)
jump _ z = Just z

current :: forall a. Zipper a -> Maybe a
current Empty = Nothing
current (Zipper _ c _) = Just c
