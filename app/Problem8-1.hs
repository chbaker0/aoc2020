-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Loops
import Control.Monad.State.Lazy
import Data.Maybe
import Debug.Trace
import qualified Data.IntSet as IntSet
import qualified Data.Vector as Vec
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

-- NOP does nothing, ACC n adjusts the accumulator, and JMP jumps to a different
-- instruction. End corresponds to the end of the program; the
-- "one-past-the-end" instruction.
data Instruction = End | Nop Int | Acc Int | Jmp Int
  deriving Show

signToMultiplier :: Char -> Int
signToMultiplier '+' = 1
signToMultiplier '-' = -1
signToMultiplier _ = undefined

codeLine :: Parser Instruction
codeLine = do
  cons <- (Nop <$ string "nop") <|> (Acc <$ string "acc") <|> (Jmp <$ string "jmp")
  space
  sign <- char '+' <|> char '-'
  arg <- read <$> many1 digit
  endOfLine
  return $ cons (arg * (signToMultiplier sign))

program :: Parser [Instruction]
program = (++ [End]) <$> manyTill codeLine eof

runTilLoop :: Vec.Vector Instruction -> Int
runTilLoop prog = runCont stateResult id
  where stateResult :: Cont Int Int
        stateResult = (`evalStateT` (0, 0, IntSet.empty)) $ callCC runTilLoop'

        runTilLoop' :: (MonadState (Int, Int, IntSet.IntSet) m) => (Int -> m ()) -> m Int
        runTilLoop' exit = do
          (off, acc, seen) <- {- traceShowId <$> -} get
          when (IntSet.member off seen) (exit acc)
          let (jmp, add) = case {-traceShowId $-} prog Vec.! off of
                             Nop _ -> (1, 0)
                             Acc n -> (1, n)
                             Jmp o -> (o, 0)
          put (off+jmp, acc+add, IntSet.insert off seen)
          runTilLoop' exit

fork :: [StateT s [] a] -> StateT s [] a
fork branches = StateT inner
  where inner s = concat $ map (`runStateT` s) branches

returnEmpty = StateT $ \s -> []

fixAndRun :: Vec.Vector Instruction -> [Int]
fixAndRun prog = evalStateT fixAndRun' (False, 0, 0, IntSet.empty)
  where fixAndRun' :: StateT (Bool, Int, Int, IntSet.IntSet) [] Int
        fixAndRun' = do
          (changed, off, acc, seen) <- {- traceShowId <$> -} get
          if IntSet.member off seen
            then returnEmpty
            else do
            put (changed, off, acc, IntSet.insert off seen)
            runIns $ prog Vec.! off

        runIns :: Instruction -> StateT (Bool, Int, Int, IntSet.IntSet) [] Int
        runIns End = gets (\(_, _, acc, _) -> acc)
        runIns (Acc n) = do
          (changed, off, acc, seen) <- get
          put (changed, off+1, acc+n, seen)
          fixAndRun'

        runIns (Nop n) = do
          (changed, off, acc, seen) <- get
          let nop = put (changed, off+1, acc, seen) >> fixAndRun'
          if changed
            then nop
            else fork [nop, put (True, off, acc, seen) >> runIns (Jmp n)]

        runIns (Jmp n) = do
          (changed, off, acc, seen) <- get
          let jmp = put (changed, off+n, acc, seen) >> fixAndRun'
          if changed
            then jmp
            else fork [jmp, put (True, off, acc, seen) >> runIns (Nop n)]

main :: IO ()
main = do
  input <- getContents
  let result = parse program "stdin" input
  putStrLn . show $ result
  let Right proglist = result
  let prog = Vec.fromList proglist
  putStrLn . show $ prog
  putStrLn . show $ runTilLoop prog
  putStrLn . show $ fixAndRun prog
