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

everyN :: Int -> [a] -> [a]
everyN n = everyN'
  where everyN' [] = []
        everyN' (x:xs) = x : (everyN' $ drop (n-1) xs)

path :: Int -> Int -> [Vec.Vector Char] -> [Char]
path right down = catMaybes . takeWhile isJust . map (uncurry (flip (Vec.!?))) . zip [0,down..] . everyN right

countTrees :: [Char] -> Int
countTrees = length . filter (== '#')

main :: IO ()
main = do
  -- Input as a list of Strings, one per row
  input <- lines <$> getContents
  -- Transpose to a list of columns, create a list of Vectors per column, and make the list of columns circular
  let cols = cycle $ map Vec.fromList $ transpose input
  let ans = product $ map (countTrees . ($ cols) . (\(r,d) -> path r d)) $ [(1,1), (3,1), (5,1), (7,1), (1,2)]
  putStrLn . show $ ans
