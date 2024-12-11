module Main (main) where

import Advent      (getInput)
import Data.IntMap (fromListWith,elems,assocs)

main =
  do inp <- getInput parse 11
     print (part1 inp)
     print (part2 inp)
  where
    parse xs = fromListWith (+) [ (read @Int x,1) | x <- words xs ]

part1 = blink 25

part2 = blink 75

step 0 = [1]
step n | xs <- show n, l <- length xs, even l, k <- l `div` 2 = map (read @Int) [take k xs,drop k xs]
       | otherwise = [n * 2024]

blink 0 m = sum (elems m)
blink i m = blink (pred i) (fromListWith (+) [ (y,n) | (x,n) <- assocs m, y <- step x ])
