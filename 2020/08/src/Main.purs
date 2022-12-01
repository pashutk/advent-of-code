module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List(..), snoc)
import Data.Maybe (Maybe(..))
import Data.Set as S
import Effect (Effect)
import Effect.Console (log, logShow)
import Input (input)
import Lib.List.Zipper (Zipper(..), forward, fromList, jump, toList)
import Lib.Parser (int)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (char, string)
import Text.Parsing.StringParser.Combinators (sepBy)

data Op = Acc Int | Jmp Int | Nop Int | Ret

instance showOp :: Show Op where
  show (Acc a) = "Acc " <> (show a)
  show (Jmp a) = "Jmp " <> (show a)
  show (Nop a) = "Nop " <> (show a)
  show (Ret) = "Ret"

parser :: Parser (List Op)
parser = sepBy parserOp (char '\n') where
  parserOffset :: Parser Int
  parserOffset = do
    s <- char '+' <|> char '-'
    o <- int 10
    pure $ o * (if s == '-' then -1 else 1)

  parserAcc = do
    _ <- string "acc "
    o <- parserOffset
    pure $ Acc o

  parserJmp = do
    _ <- string "jmp "
    o <- parserOffset
    pure $ Jmp o

  parserNop = do
    _ <- string "nop "
    o <- parserOffset
    pure $ Nop o

  parserOp :: Parser Op
  parserOp = parserAcc <|> parserJmp <|> parserNop

data Reason = OutOfBoundAccess | InfiniteLoop

data ExecResult = Success Int | Fail Reason Int Int

instance showExecResult :: Show ExecResult where
  show (Success acc) = "Success, result: " <> (show acc)
  show (Fail InfiniteLoop acc pos) = "Infinite loop, last acc value: " <> (show acc) <> ", next pos: " <> (show pos)
  show (Fail OutOfBoundAccess acc pos) = "Out of bound, last acc value: " <> (show acc) <> ", next pos: " <> (show pos)

main :: Effect Unit
main = do
  let
    parsed = runParser parser input
    ops = case parsed of
      Left _ -> Nil
      Right a -> a

    prog = snoc ops Ret
      
    interpret ops' = f {zipper: fromList ops', pos: 0} 0 S.empty where
      f :: { zipper :: Zipper Op, pos :: Int} -> Int -> S.Set Int -> ExecResult
      f {zipper: Empty} _ _ = Success 0
      f z acc visitedPos | S.member z.pos visitedPos = Fail InfiniteLoop acc z.pos
      f {zipper: z@(Zipper back (Nop _) front), pos} acc visitedPos = case forward z of
        Just nz -> f {zipper: nz, pos: pos + 1} acc (S.insert pos visitedPos)
        Nothing -> Fail OutOfBoundAccess acc pos
      f {zipper: z@(Zipper _ (Acc a) _), pos} acc visitedPos = case forward z of
        Just nz -> f {zipper: nz, pos: pos + 1} (acc + a) (S.insert pos visitedPos)
        Nothing -> Fail OutOfBoundAccess acc pos
      f {zipper: z@(Zipper _ (Jmp a) _), pos} acc visitedPos = case jump a z of
        Just nz -> f {zipper: nz, pos: pos + a} acc (S.insert pos visitedPos)
        Nothing -> Fail OutOfBoundAccess acc pos
      f {zipper: (Zipper _ Ret _)} acc _ = Success acc

    secondTask = f $ fromList prog where
      next :: Zipper Op -> Either String Int
      next z = case forward z of
        Just nz -> f nz
        Nothing -> Left "End of ops"

      f :: Zipper Op -> Either String Int
      f Empty = Left "Empty ops"
      f z@(Zipper back (Nop offset) front) = case interpret $ toList (Zipper back (Jmp offset) front) of
        Success a -> Right a
        Fail _ _ _ -> next z
      f z@(Zipper back (Jmp offset) front) = case interpret $ toList (Zipper back (Nop offset) front) of
        Success a -> Right a
        Fail _ _ _ -> next z
      f z@(Zipper _ (Acc _) _) = next z
      f z@(Zipper _ Ret _) = Left "Got ret before success"

  log "First task:"
  logShow $ interpret ops

  log "Second task:"
  logShow secondTask
