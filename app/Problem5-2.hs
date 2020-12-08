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
import Control.Monad

parseBinary :: String -> Maybe Int
parseBinary = parseDigits >=> (return . foldl' (\acc x -> 2 * acc + x) 0)
  where parseDigits = sequence . map digit
        digit '0' = Just 0
        digit '1' = Just 1
        digit _ = Nothing

splitComponents :: String -> (String, String)
splitComponents = liftM2 (,) (take 7) (drop 7)

rowToBinary :: String -> Maybe String
rowToBinary = sequence . map digit
  where digit 'F' = Just '0'
        digit 'B' = Just '1'
        digit _ = Nothing

colToBinary :: String -> Maybe String
colToBinary = sequence . map digit
  where digit 'L' = Just '0'
        digit 'R' = Just '1'
        digit _ = Nothing

parseSeat :: String -> Maybe (Int, Int)
parseSeat str = do
  let (rowStr, colStr) = splitComponents str
  row <- rowToBinary rowStr >>= parseBinary
  col <- colToBinary colStr >>= parseBinary
  return (row, col)

seatID :: (Int, Int) -> Int
seatID (row, col) = row * 8 + col

findGap :: [Int] -> Maybe Int
findGap [] = Nothing
findGap (_:[]) = Nothing
findGap (x:y:rest) =
  case y-x of 1 -> findGap (y:rest)
              2 -> Just (x+1)
              _ -> Nothing

main :: IO ()
main = do
  input <- lines <$> getContents
  let Just seats = sort <$> (sequence $ map (liftM seatID . parseSeat) input)
  putStrLn . show $ seats
  putStrLn . show $ findGap seats
