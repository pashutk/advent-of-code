module Main where

import Prelude

import Control.Alt (alt, (<|>))
import Data.Array (fromFoldable)
import Data.Char (toCharCode)
import Data.Either (isRight)
import Data.Foldable (elem, fold, foldl)
import Data.List (List, filter, findIndex, (..))
import Data.List as L
import Data.Maybe (isNothing)
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (log)
import Input (input)
import Lib.Parser (digit, emptyLine, int, manyAs1, oneConsecChar)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodeUnits (alphaNum, char, satisfy, string)
import Text.Parsing.StringParser.Combinators (many, many1, sepBy)

data Pair = BirthYear String
  | PassportID String
  | IssueYear String
  | ExpirationYear String
  | Height String
  | HairColor String
  | EyeColor String
  | CountryID String

-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID)

instance showPair :: Show Pair where
  show (BirthYear a) = "BirthdayYear " <> (show a)
  show (PassportID a) = "PassportId " <> (show a)
  show (IssueYear a) = "IssueYear " <> (show a)
  show (ExpirationYear a) = "ExpirationYear " <> (show a)
  show (Height a) = "Height " <> (show a)
  show (HairColor a) = "HairColor " <> (show a)
  show (EyeColor a) = "EyeColor " <> (show a)
  show (CountryID a) = "CountryID " <> (show a)

parserKey :: Parser String
parserKey = string "byr" <|>
  string "iyr" <|>
  string "eyr" <|>
  string "hgt" <|>
  string "hcl" <|>
  string "ecl" <|>
  string "pid" <|>
  string "cid"

type PairT = Tuple String String

type Passport = List PairT

parserPair :: Parser PairT
parserPair = do
  k <- parserKey
  _ <- char ':'
  v <- (many $ alphaNum <|> (char '#'))
  pure $ Tuple k $ fold $ map singleton v

parser :: Parser (List Passport)
parser = sepBy parserRow emptyLine where
  parserPairSep = manyAs1 ' ' <|> oneConsecChar '\n'
  parserRow = sepBy parserPair parserPairSep

isValidPassportWeak :: Passport -> Boolean
isValidPassportWeak p = has "byr" p
  && has "iyr" p
  && has "eyr" p
  && has "hgt" p
  && has "hcl" p
  && has "ecl" p
  && has "pid" p
  where
    has :: String -> Passport -> Boolean
    has k = map fst >>> findIndex (_ == k) >>> not isNothing

data Height = Centimeteres Int | Inches Int

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
--  If cm, the number must be at least 150 and at most 193.
--  If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.

isValidPassport :: Passport -> Boolean
isValidPassport = filter f >>> isValidPassportWeak where
  check :: forall a. Parser a -> String -> Boolean
  check p = runParser p >>> isRight

  parserYear :: Int -> Int -> Parser Int
  parserYear min max = do
    a <- int 10
    case a >= min && a <= max of
      true -> pure a
      false -> fail $ "Expected year between " <> show min <> " and " <> show max

  parserHeight :: { minCm :: Int, maxCm :: Int, minIn :: Int, maxIn :: Int } -> Parser Height
  parserHeight r = do
    h <- int 10
    u <- string "cm" <|> string "in"
    case u of
      "cm" -> if h >= r.minCm && h <= r.maxCm 
        then pure $ Centimeteres h
        else fail $ showRangeError "cm" r.minCm r.maxCm
      "in" -> if h >= r.minIn && h <= r.maxIn
        then pure $ Inches h
        else fail $ showRangeError "in" r.minCm r.maxCm
      _ -> fail "Expected height unit: cm or in"
    where
      showRangeError u min max = "Expected height in " <> show u <> " in range " <> show min <> " and " <> show max

  parserHexColor :: Parser String
  parserHexColor = do
    v1 <- char '#'
    v2 <- parserHex
    v3 <- parserHex
    v4 <- parserHex
    v5 <- parserHex
    v6 <- parserHex
    v7 <- parserHex
    pure $ fromCharArray [v1, v2, v3, v4, v5, v6, v7]
    where
      parserHex = satisfy $ toCharCode >>> flip elem ((48 .. 57) <> (65 .. 70) <> (97 .. 102))

  parserEyeColor :: Parser String
  parserEyeColor = foldl alt (string "amb") $ map string ["blu", "brn", "gry", "grn", "hzl", "oth"]

  parserFixedLengthDigitString :: Int -> Parser String
  parserFixedLengthDigitString l = try do
    a <- fromCharArray <$> fromFoldable <$> many1 digit
    case SCU.length a of
      x | x == 9 -> pure a
      _ -> fail $ "Expected length is " <> show l

  f (Tuple "byr" s) = check (parserYear 1920 2002) s
  f (Tuple "iyr" s) = check (parserYear 2010 2020) s
  f (Tuple "eyr" s) = check (parserYear 2020 2030) s
  f (Tuple "hgt" s) = check (parserHeight { minCm: 150, maxCm: 193, minIn: 59, maxIn: 76 }) s
  f (Tuple "hcl" s) = check parserHexColor s
  f (Tuple "ecl" s) = check parserEyeColor s
  f (Tuple "pid" s) = check (parserFixedLengthDigitString 9) s
  f _               = false

main :: Effect Unit
main = do
  log "First task"
  log $ show $ L.length <$> do
    a <- runParser parser input
    pure $ filter isValidPassportWeak a
  
  log "Second task"
  log $ show $ L.length <$> do
    a <- runParser parser input
    pure $ filter isValidPassport a
