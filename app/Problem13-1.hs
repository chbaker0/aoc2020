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

import Control.Arrow
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

data Schedule = Schedule Int [Maybe Int]
  deriving Show

schedule :: Parser Schedule
schedule = do
  earliest <- read <$> manyTill digit endOfLine
  buses <- (flip sepBy1) (char ',') $ Just . read <$> many1 digit <|> Nothing <$ char 'x'
  return $ Schedule earliest buses

compareOnSnd :: Ord a => (b, a) -> (b, a) -> Ordering
compareOnSnd (_, x) (_, y) = compare x y

solve1 :: Schedule -> Int
solve1 (Schedule e buses) = id * wait
  where (id, wait) = traceShowId $ minimumBy compareOnSnd $ map (\n -> (n, n - (e `mod` n))) $ catMaybes buses

main :: IO ()
main = do
  Right schedule <- traceShowId . parse schedule "stdin" <$> getContents
  putStrLn . show $ solve1 schedule
