module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (all, any, fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.List (List(..), drop, span)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Input (input)
import Lib.Graph (Graph, fromMap, lookup, outEdges, topologicalSort)
import Lib.Parser (int)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (anyLetter, char, string, whiteSpace)
import Text.Parsing.StringParser.Combinators (many, optional, sepBy)

parserWord :: Parser String
parserWord = map (fromFoldable >>> fromCharArray) do
  a <- anyLetter
  bs <- many anyLetter
  pure $ Cons a bs

-- Color is two words separated by whitespace
parserColor :: Parser String
parserColor = do
  a <- parserWord
  w <- char ' ' $> " "
  b <- parserWord
  pure $ a <> w <> b

type CountedBags = { color :: String, count :: Int}

type Rule = { color :: String, children :: List CountedBags }

parserCountedBags :: Parser CountedBags
parserCountedBags = do
  n <- int 10
  _ <- char ' '
  c <- parserColor
  _ <- string " bag"
  _ <- optional $ char 's'
  pure $ { color: c, count: n }

parserNoBags :: Parser (List CountedBags)
parserNoBags = string "no other bags" $> Nil

parserRule :: Parser Rule
parserRule = do
  c <- parserColor
  _ <- string " bags contain "
  cbs <- parserNoBags <|> sepBy parserCountedBags (string ", ")
  _ <- char '.'
  pure $ { color: c, children: cbs }

parser :: Parser (List Rule)
parser = sepBy parserRule whiteSpace

main :: Effect Unit
main = do
  let
    contentBag = "shiny gold"

    parsed = runParser parser input

    rules = case parsed of
      Left _ -> Nil
      Right a -> a
    
    graph :: Graph String (List CountedBags)
    graph = fromMap $ M.fromFoldable $ map ruleToMapElem rules where
      ruleToMapElem r = Tuple r.color $ Tuple r.children $ _.color <$> r.children

    { init: possibleColors, rest: impossibleColors } = span (_ /= contentBag) $ topologicalSort graph

    impossibleColorsSet = S.fromFoldable $ drop 1 impossibleColors

    -- Find all possible out keys (keys which are contentBag or leads to one), then remove contentBag key
    -- Won't terminate if contentBag not in graph
    firstResult = S.size $ S.delete contentBag (f possibleColors impossibleColorsSet (S.singleton contentBag) Nil) where
      f (Cons h t) impossibleOut possibleOut rest = case outEdges h graph of
        Just l@(Cons _ _) | any (flip S.member possibleOut) l   -> f t impossibleOut              (S.insert h possibleOut) rest
        Just l@(Cons _ _) | all (flip S.member impossibleOut) l -> f t (S.insert h impossibleOut) possibleOut              rest
        Just l@(Cons _ _)                                       -> f t impossibleOut              possibleOut              (Cons h rest)
        _                                                       -> f t (S.insert h impossibleOut) possibleOut              rest
      f Nil impossibleOut possibleOut Nil = possibleOut
      f Nil impossibleOut possibleOut rest = f rest impossibleOut possibleOut Nil

    secondResult = f contentBag where
      f k = case lookup k graph of
        Just l@(Cons _ _) -> sum $ (\{color, count} -> count * (f color) + count) <$> l
        _ -> 0

  log "First task:"
  logShow firstResult

  log "Second task:"
  logShow secondResult
