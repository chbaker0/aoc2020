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
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Graph as Graph
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

data Rule = Rule
  { outerBag :: String
  , innerBags :: [(Int, String)]
  }
  deriving Show

contained :: Parser [(Int, String)]
contained = ((try $ string "no other bags.") >> return []) <|> do
  num <- read . (:[]) {- eat the digit-} <$> digit
  space
  bag <- manyTill anyChar (try $ string " bag")
  optional $ char 's'
  sep <- anyChar
  case sep of
    ',' -> space >> (((num, bag):) <$> contained)
    '.' -> return [(num, bag)]

rule :: Parser Rule
rule = do
  outer <- manyTill anyChar (try (string " bags "))
  string "contain "
  inner <- contained
  endOfLine
  return $ Rule outer inner

ruleset :: Parser [Rule]
ruleset = manyTill rule eof

data BagGraph = BagGraph
  { innerGraph :: Graph.Graph
  , ruleFromVertex :: Graph.Vertex -> Rule
  , vertexFromBag :: String -> Maybe Graph.Vertex
  }

graphiphy :: [Rule] -> BagGraph
graphiphy rules = BagGraph graph ruleFromVertex vertexFromKey
  where edgeList = map (\r -> (r, outerBag r, map snd $ innerBags r)) rules
        (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges edgeList
        ruleFromVertex = (\(r, _, _) -> r) . nodeFromVertex

listContainingBags :: String -> BagGraph -> [String]
listContainingBags bag graph = map (outerBag . ruleFromVertex graph) . Graph.reachable transposed $ srcVertex
  where transposed = Graph.transposeG $ innerGraph graph
        srcVertex = fromJust $ vertexFromBag graph bag

countContainedBags :: String -> [Rule] -> Integer
countContainedBags bag rules = (traceShow counts $ fromJust $ Map.lookup bag counts) - 1
  where counts :: Map.Map String Integer
        counts = Map.fromList [(outerBag r, 1 + go r) | r <- rules]

        go :: Rule -> Integer
        go rule = foldl' (\b (m, n) -> b + m*n) (toInteger 0)
                  $ map (\(m, b) -> (toInteger m, fromJust $ Map.lookup b counts))
                  $ innerBags rule

main :: IO ()
main = do
  input <- getContents
  let result = parse ruleset "stdin" input
  putStrLn . show $ result
  let Right rules = result
  let containing = listContainingBags "shiny gold" $ graphiphy rules
  putStrLn . show $ containing
  -- `containing` includes "shiny gold" itself so subtract 1
  let ans1 = (length containing) - 1
  putStrLn . show $ ans1

  let ans2 = countContainedBags "shiny gold" rules
  putStrLn . show $ ans2
