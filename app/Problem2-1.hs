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

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

data Policy = Policy
  { character :: Char
  , minnum :: Int
  , maxnum :: Int
 }
  deriving (Show)

parseInt :: GenParser Char () Int
parseInt = read <$> many1 digit

parseQuery :: GenParser Char () (Policy, String)
parseQuery = do
  min <- parseInt
  char '-'
  max <- parseInt
  space
  character <- letter
  char ':'
  space
  password <- many1 letter
  return (Policy character min max, password)

validateQuery :: GenParser Char () Bool
validateQuery = isValid <$> parseQuery

countValid :: GenParser Char () Int
countValid = (length . filter id) <$> endBy validateQuery (((\_ -> ()) <$> endOfLine) <|> eof)

isValid :: (Policy, String) -> Bool
isValid (pol, pwd) = c >= (minnum pol) && c <= (maxnum pol)
  where c = length $ filter (==character pol) $ pwd

fmtOutput :: Show a => Either ParseError a -> String
fmtOutput (Left err) = show err
fmtOutput (Right x) = show x

main :: IO ()
main = do
  input <- getContents
  let result = runParser countValid () "stdin" input
  putStrLn . fmtOutput $ result
