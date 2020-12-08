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
import Data.Maybe
import qualified Data.Vector as Vec

everyThird :: [a] -> [a]
everyThird = map (\(_,x) -> x) . filter (\(i,_) -> i == 0) . zip (cycle [0,1,2])

path :: [Vec.Vector Char] -> [Char]
path = catMaybes . takeWhile isJust . map (uncurry (flip (Vec.!?))) . zip [0..] . everyThird

countTrees :: [Char] -> Int
countTrees = length . filter (== '#')

main :: IO ()
main = do
  -- Input as a list of Strings, one per row
  input <- lines <$> getContents
  -- Transpose to a list of columns, create a list of Vectors per column, and make the list of columns circular
  let cols = cycle $ map Vec.fromList $ transpose input
  putStrLn . show $ countTrees . path $ cols
