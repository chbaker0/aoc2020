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

import Data.List
import qualified Data.Vector as Vec
import Debug.Trace

count x = length . filter (== x)

sortAdapters xs = ans
  where ans = sort (0:xs) ++ [3 + (last $ init ans)]

solve1 :: [Int] -> Int
solve1 adapters = (count 1 gaps) * (count 3 gaps)
  where gaps = map (\(a, b) -> b-a) $ zip sorted (tail sorted)
        sorted = sortAdapters adapters

solve2 :: [Int] -> Integer
solve2 adapters = dp Vec.! (length sorted - 1)
  where dp :: Vec.Vector Integer
        dp = Vec.fromList $ 1 : [sum [if a-b <= 3 then dp Vec.! j else 0 | (j, b) <- take i indexed] | (i, a) <- tail indexed]
        indexed = zip [0..] sorted
        sorted = sortAdapters adapters

main :: IO ()
main = do
  adapters <- map read . lines <$> getContents
  putStrLn . show $ solve1 adapters
  putStrLn . show $ solve2 adapters
