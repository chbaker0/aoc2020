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
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
countryField = "cid"
allFields = countryField : requiredFields

parseField :: GenParser Char () (String, String)
parseField = do
  notFollowedBy endOfLine
  label <- choice . map (try . string) $ allFields
  char ':'
  info <- manyTill anyChar space
  return (label, info)

parsePassport :: GenParser Char () (Map.Map String String)
parsePassport = Map.fromList <$> manyTill parseField (endOfLine)

parseList :: GenParser Char () [Map.Map String String]
parseList = manyTill parsePassport eof

fmtOutput :: Show a => Either ParseError a -> String
fmtOutput (Left err) = show err
fmtOutput (Right x) = show x

isValid :: Map.Map String String -> Bool
isValid m = let presentFields = filter (isJust . (flip Map.lookup $ m)) allFields
            in intersect presentFields requiredFields == requiredFields

main :: IO ()
main = do
  result <- runParser parseList () "stdin" <$> getContents
  putStrLn . show $ result
  -- let numValid = length . filter isValid $ passports
  -- putStrLn . show $ numValid
