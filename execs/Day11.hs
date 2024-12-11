module Main (main) where

import Advent          (getInput)
import Data.Foldable   (foldl')
import Data.List       (group,sort)
import Data.Map.Strict (fromList,(!),insertWith)

main =
  do inp <- getInput parse 11
     print (part1 inp)
     print (part2 inp)
  where
    parse = map (read @Int) . words

part1 = blink 25

part2 = blink 75

step 0 = [1]
step n | xs <- show n, l <- length xs, even l, k <- l `div` 2 = map (read @Int) [take k xs,drop k xs]
       | otherwise = [n * 2024]

blink k xs = go k 0 (fromList xs') [ n | ((n,0),_) <- xs' ]
  where
    xs' = [ ((n,0),length ns) | ns@(n:_) <- group (sort xs) ]

go k i a xs
  | i == k    = sum [ a ! (x,k) | x <- xs ]
  | otherwise = go k i' a' xs'
  where
    i'  = succ i
    ys  = [ ((n,i'),mul) | x <- xs, let mul = a ! (x,i), n <- step x ]
    a'  = foldl' (\m (k,v) -> insertWith (+) k v m) a ys
    xs' = [ x | (x:_) <- group (sort [ x | ((x,_),_) <- ys ]) ]
