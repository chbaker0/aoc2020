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
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

data Direction = DirNorth | DirSouth | DirEast | DirWest
  deriving Show

data Action = North | South | East | West | TurnLeft | TurnRight | Forward
  deriving Show

data Navigation = Navigation Action Int
  deriving Show

parseNavigations :: Parser [Navigation]
parseNavigations = (`manyTill` eof) $ do
  action <- North <$ char 'N' <|> South <$ char 'S' <|> East <$ char 'E' <|> West <$ char 'W'
            <|> TurnLeft <$ char 'L' <|> TurnRight <$  char 'R' <|> Forward <$ char 'F'
  value <- read <$> manyTill digit endOfLine
  return $ Navigation action value

directionToAction DirNorth = North
directionToAction DirSouth = South
directionToAction DirEast = East
directionToAction DirWest = West

clockwise DirNorth = DirEast
clockwise DirEast = DirSouth
clockwise DirSouth = DirWest
clockwise DirWest = DirNorth

counterclockwise = clockwise . clockwise . clockwise

solve1 :: [Navigation] -> Int
solve1 navs = abs x + abs y
  where (x, y) = traceShowId $ foldl' (flip go) (0, 0) $ traceShowId withDir

        go :: (Navigation, Direction) -> (Int, Int) -> (Int, Int)
        go (Navigation North n, _) (x, y) = (x, y+n)
        go (Navigation South n, _) (x, y) = (x, y-n)
        go (Navigation East n,  _) (x, y) = (x+n, y)
        go (Navigation West n,  _) (x, y) = (x-n, y)
        go (Navigation Forward n, d) p = go (Navigation (directionToAction d) n, d) p
        go _ p = p

        withDir :: [(Navigation, Direction)]
        withDir = zip navs $ scanl (flip turn) DirEast navs

        turn (Navigation TurnLeft x) = iterate (counterclockwise .) id !! div x 90
        turn (Navigation TurnRight x) = iterate (clockwise .) id !! div x 90
        turn _ = id

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate p 0 = p
rotate (x, y) n = rotate (y, -x) (n-1)

solve2 :: [Navigation] -> Int
solve2 navs = abs x + abs y
  where (x, y) = traceShowId $ foldl' (uncurry . goShip) (0, 0) withWaypoint
        withWaypoint = traceShowId $ (flip zip) navs $ scanl goWay (10, 1) navs

        goWay :: (Int, Int) -> Navigation -> (Int, Int)
        goWay p (Navigation TurnRight n) = rotate p (div n 90)
        goWay p (Navigation TurnLeft n) = rotate p (360 - div n 90)
        goWay (x, y) (Navigation North n) = (x, y+n)
        goWay (x, y) (Navigation South n) = (x, y-n)
        goWay (x, y) (Navigation East n)  = (x+n, y)
        goWay (x, y) (Navigation West n)  = (x-n, y)
        goWay p (Navigation Forward _) = p

        goShip :: (Int, Int) -> (Int, Int) -> Navigation -> (Int, Int)
        goShip (sx, sy) (wx, wy) (Navigation Forward n) = (sx + n*wx, sy + n*wy)
        goShip p _ _ = p

main :: IO ()
main = do
  Right navs <- traceShowId . parse parseNavigations "stdin" <$> getContents
  putStrLn . show $ solve1 navs
  putStrLn . show $ solve2 navs
