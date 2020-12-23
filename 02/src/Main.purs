module Main where

import Prelude

import Data.Foldable (foldl)
import Data.List as List
import Data.List.NonEmpty (filter, (!!))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Input (input)
import Lib.Parser (int)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (anyLetter, char, string)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy)

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
        pair <- parserPair (int 10) (char '-')
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