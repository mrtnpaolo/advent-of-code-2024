module Main (main) where

import Advent (getInputLines, count)

main =
  do inp <- getInputLines (map (read @Int) . words) 2
     print (part1 inp)
     print (part2 inp)

part1 = count safe

safe xs = (all (<0) ds || all (>0) ds) && all (\n -> 1 <= n && n <= 3) (map abs ds)
  where
    ds = zipWith (-) xs (tail xs)

part2 = count (any safe . alts)

alts []     = [[]]
alts (x:xs) = xs : map (x:) (alts xs)
