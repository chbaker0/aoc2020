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

module Main where

import Control.Monad
import qualified Data.Graph as G
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import Data.List.Extra
import Data.Maybe
import Data.Ord
import qualified Data.Vector as Vec
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

-- A CFG production rule. Nonterminals are identified by Ints
data Production = Term Char | Nonterm [Int]
  deriving Show

type Rule = (Int, [Production])

number :: Parser Int
number = read <$> many1 digit

terminal :: Parser Char
terminal = char '"' *> anyChar <* char '"'

nonterminal :: Parser [[Int]]
nonterminal = (`sepBy1` string "| ") $ sepEndBy1 number (char ' ')

rules :: Parser [Rule]
rules = (`manyTill` endOfLine) $ do
  ident <- number
  string ": "
  res <- ((,) ident . (:[]) . Term) <$> terminal <|>
         ((,) ident) . map Nonterm <$> nonterminal
  endOfLine
  return res

-- Parse the input, returning a pair of (grammar rules, strings to check)
rulesAndMessages :: Parser ([Rule], [String])
rulesAndMessages = do
  rs <- rules
  strs <- (`manyTill` eof) $ (`manyTill` endOfLine) letter
  return (rs, strs)

-- Bastardized CYK algorithm. A bit slow; takes about 1 minute on my machine to
-- check all strings.
checkStr :: [Rule] -> String -> Bool
checkStr rs str = fromJust $ M.lookup (0, Vec.length input, 0) isMatch
  where input = Vec.fromList str
        rules = IM.fromList rs

        -- Whether the given range of input matches the given rule. (i, j, r) ->
        -- Bool where [i, j) indexes `input` and r is a rule ID in `rs`.
        isMatch :: M.Map (Int, Int, Int) Bool
        isMatch = M.fromList $ map (\(i, j, r) -> ((i, j, r), go i j r))
                  [(i, j, r) | (r, _) <- rs, i <- [0..Vec.length input - 1], j <- [i+1..Vec.length input]]

        go :: Int -> Int -> Int -> Bool
        go i j r = isJust . find id $ map (go' i j) $ fromJust $ IM.lookup r rules

        go' :: Int -> Int -> Production -> Bool
        go' i j (Term c)
          | j == i+1 = c == input Vec.! i
          | otherwise = False
        go' i j (Nonterm [s]) = fromJust $ M.lookup (i, j, s) isMatch
        go' i j (Nonterm (s:ss)) = isJust . find id $
          [(fromJust $ M.lookup (i, mid, s) isMatch) && go' mid j (Nonterm ss) | mid <- [i+1..j-1]]

fixRules :: [Rule] -> [Rule]
fixRules rs = fixup
  where rmap = IM.fromList $ rs
        fixup = IM.toList . ($ rmap) $
                IM.insert 8 [Nonterm [42], Nonterm [42, 8]] . IM.insert 11 [Nonterm [42, 31], Nonterm [42, 11, 31]]

main :: IO ()
main = do
  Right (rs, strs) <- traceShowId . parse rulesAndMessages "stdin" <$> getContents
  -- Part 1:
  -- let matches = length . filter id $ map (checkStr rs) strs
  -- putStrLn . show $ matches
  -- Part 2:
  let matches = length . filter id $ map (checkStr $ fixRules rs) strs
  putStrLn . show $ matches
