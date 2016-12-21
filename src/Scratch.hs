module Scratch where

import           Data.Char (digitToInt, isDigit)

s :: Either String Int
s = Left "foo"

n :: Either String Int
n = Right 3

x0 = fmap (*2) s
x1 = fmap (*2) n

parseEither :: Char -> Either String Int
parseEither c
  | isDigit c = Right (digitToInt c)
  | otherwise = Left "parse error"

parseMultiple :: Either String Int
parseMultiple = do
  x <- parseEither '1'
  y <- parseEither '2'
  return (x + y)

parseMultiple' :: Either String Int
parseMultiple' = do
  x <- parseEither 'm'
  y <- parseEither '2'
  return (x + y)
