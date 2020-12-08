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

import Control.Monad
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

parseDeclaration :: Parser (Set.Set Char)
parseDeclaration = Set.fromList <$> manyTill letter endOfLine

parseGroup :: Parser (Set.Set Char)
parseGroup = Set.unions <$> manyTill parseDeclaration ((void endOfLine) <|> eof)

parseAll :: Parser [Set.Set Char]
parseAll = manyTill parseGroup eof

main :: IO ()
main = do
  input <- getContents
  -- let result = parse parseAll "stdin" input
  -- putStrLn . show $ result
  let Right groups = parse parseAll "stdin" input
  let ans = sum . map Set.size $ groups
  putStrLn . show $ ans
