module Main (main) where

import Advent     (getInputLines, count)
import Data.List  (sort)

main =
  do inp <- unzip <$> getInputLines parse 1
     print (part1 inp)
     print (part2 inp)
  where
    parse (words -> [a,b]) = (read @Int a,read @Int b)

part1 (xs,ys) = sum $ map abs $ zipWith (-) (sort xs) (sort ys)

part2 (xs,ys) = sum [ n * count (n==) ys | n <- xs ]
