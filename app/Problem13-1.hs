module Main where

import Control.Arrow
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

data Schedule = Schedule Int [Maybe Int]
  deriving Show

schedule :: Parser Schedule
schedule = do
  earliest <- read <$> manyTill digit endOfLine
  buses <- (flip sepBy1) (char ',') $ Just . read <$> many1 digit <|> Nothing <$ char 'x'
  return $ Schedule earliest buses

compareOnSnd :: Ord a => (b, a) -> (b, a) -> Ordering
compareOnSnd (_, x) (_, y) = compare x y

solve1 :: Schedule -> Int
solve1 (Schedule e buses) = id * wait
  where (id, wait) = traceShowId $ minimumBy compareOnSnd $ map (\n -> (n, n - (e `mod` n))) $ catMaybes buses

main :: IO ()
main = do
  Right schedule <- traceShowId . parse schedule "stdin" <$> getContents
  putStrLn . show $ solve1 schedule
