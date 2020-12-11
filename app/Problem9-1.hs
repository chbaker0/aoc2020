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

import qualified Data.IntSet as IntSet
import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Debug.Trace

findPair :: Int -> IntSet.IntSet -> Maybe (Int, Int)
findPair n s = lower >>= (\x -> Just (x, n-x))
  where lower = find (\x -> traceShowId $ IntSet.member (n-x) s) $ traceShowId $ IntSet.toAscList s

process :: Int -> Int -> State IntSet.IntSet Bool
process fstPre n = do
  valid <- isJust . findPair n <$> get
  modify (traceShowId . IntSet.insert n . IntSet.delete fstPre)
  return valid

solve :: Int -> [Int] -> Maybe Int
solve preLen numbers = (amble!!) <$> findIndex not valids
  where initSet = traceShowId $ IntSet.fromList $ take preLen numbers
        amble = drop preLen numbers
        window = zip numbers amble
        valids = traceShowId $ (`evalState` initSet) $ sequence $ map (uncurry process) window

solve2 :: Int -> [Int] -> [Int]
solve2 s xs = take (j-i+1) $ drop i xs
  where presum = tail $ scanl' (+) 0 xs
        (i, j) = join go $ zip3 [0..] xs presum

        go ((i,x,px):xs) ((j,y,py):ys) =
          if i == j then go ((i,x,px):xs) ys else
            case compare (py-px+x) s of
              EQ -> (i,j)
              LT -> go ((i,x,px):xs) ys
              GT -> go xs ((j,y,py):ys)

main :: IO ()
main = do
  numbers <- map read . lines <$> getContents
  let Just inv = solve 25 numbers
  putStrLn . show $ inv
  let xs = solve2 inv numbers
  let (a, b) = let xs' = sort xs in (head xs', last xs')
  putStrLn . show $ xs
  putStrLn $ show a ++ " " ++ show b ++ " " ++ show (a+b)
