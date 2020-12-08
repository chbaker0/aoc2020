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
  , pos1 :: Int
  , pos2 :: Int
 }
  deriving (Show)

parseInt :: GenParser Char () Int
parseInt = read <$> many1 digit

parseQuery :: GenParser Char () (Policy, String)
parseQuery = do
  fst <- parseInt
  char '-'
  snd <- parseInt
  space
  character <- letter
  char ':'
  space
  password <- many1 letter
  return (Policy character fst snd, password)

validateQuery :: GenParser Char () Bool
validateQuery = isValid <$> parseQuery

countValid :: GenParser Char () Int
countValid = (length . filter id) <$> endBy validateQuery (((\_ -> ()) <$> endOfLine) <|> eof)

isValid :: (Policy, String) -> Bool
isValid (pol, pwd) = (c1 == character pol) /= (c2 == character pol)
  where c1 = pwd !! (pos1 pol - 1)
        c2 = pwd !! (pos2 pol - 1)

fmtOutput :: Show a => Either ParseError a -> String
fmtOutput (Left err) = show err
fmtOutput (Right x) = show x

main :: IO ()
main = do
  input <- getContents
  let result = runParser countValid () "stdin" input
  putStrLn . fmtOutput $ result
