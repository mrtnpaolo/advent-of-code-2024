module Main (main) where

import Advent             (getInputArray,cardinal,count,bfsOn)
import Data.Array.Unboxed (assocs,(!),(!?))

main =
  do inp <- getInputArray 10
     print (part1 inp)
     print (part2 inp)

part1 m = sum scores
  where
    starts  = [ c | (c,'0') <- assocs m ]
    nexts c = [ d | d <- cardinal c, Just y <- [m !? d], succ (m ! c) == y ]
    scores  = [ count (\c -> m ! c == '9') (bfsOn id nexts [start]) | start <- starts ]

part2 m = ratings
  where
    starts         = [ [c] | (c,'0') <- assocs m ]
    nexts cs@(c:_) = [ (d:cs) | d <- cardinal c, Just y <- [m !? d], succ (m ! c) == y ]
    ratings        = count (\(c:_) -> m ! c == '9') (bfsOn id nexts starts)
