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

import Lib

import Data.List
import qualified Data.IntSet as IntSet

parse_list :: Read a => String -> [a]
parse_list = map read . lines

pairs :: [a] -> [(a, a)]
pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

solve :: [Int] -> Maybe (Int, Int, Int)
solve xs = s >>= (\(a,b) -> Just (a, b, 2020-a-b))
  where leftovers = IntSet.fromList . map (2020 -) $ xs
        s = find (\(a,b) -> IntSet.member (a+b) leftovers) $ pairs xs

main :: IO ()
main = do
  input <- getContents
  let input_list = parse_list input
  let solution = maybe (-1) (\(a,b,c) -> a*b*c) $ solve input_list
  putStrLn $ show solution
