module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (pow)
import Data.List as List
import Data.List.NonEmpty (filter, length, (!!))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Input (input)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (anyLetter, char, string)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy)

parserDigit :: Parser Int
parserDigit = string "0" $> 0
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
parserInt :: Int -> Parser Int
parserInt radix = f <*> length <$> many1 parserDigit where
    f ns l = foldrWithIndex f' 0 ns where
        f' i a b = pow radix (l - i - 1) * a + b

-- Pair is two values separated by separator
parserPair :: forall a sep. Parser a -> Parser sep -> Parser (Pair a)
parserPair a sep = do
    start <- a
    _ <- sep
    end <- a
    pure (Tuple start end)

type Pair a = Tuple a a

type Policy = { letter :: Char, pair :: Pair Int }

type Line = { policy :: Policy, password :: NonEmptyList Char }

main :: Effect Unit
main = log $ show $ f input where
    parserLine :: Parser Line
    parserLine = do
        pair <- parserPair (parserInt 10) (char '-')
        _ <- char ' '
        letter <- anyLetter
        _ <- string ": "
        password <- many1 anyLetter
        pure { policy: {letter: letter, pair: pair}, password: password }

    countLetter :: Char -> NonEmptyList Char -> Int
    countLetter letter = filter (_ == letter) >>> List.length

    inRange :: Pair Int -> Int -> Boolean
    inRange r a = a >= fst r && a <= snd r

    rule1 :: Line -> Boolean
    rule1 line = inRange line.policy.pair $ countLetter line.policy.letter line.password

    rule2 :: Line -> Boolean
    rule2 line = case rule2' line of
        Just x -> x
        Nothing -> false

    rule2' :: Line -> Maybe Boolean
    rule2' line = do
        fstChar <- line.password !! (fst line.policy.pair - 1)
        sndChar <- line.password !! (snd line.policy.pair - 1)
        pure ((fstChar == line.policy.letter) /= (sndChar == line.policy.letter))
    
    fld :: (Line -> Boolean) -> Int -> Line -> Int
    fld check acc cur = acc + (if check cur then 1 else 0)

    parserLines :: Parser (List.List Line)
    parserLines = sepEndBy parserLine $ char '\n'

    f i = Tuple fstResult sndResult where
        lines = runParser parserLines i
        fstResult = foldl (fld rule1) 0 <$> lines
        sndResult = foldl (fld rule2) 0 <$> lines