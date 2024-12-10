module Main (main) where

import Advent             (getInputArray,cardinal,count,bfsOn)
import Data.Array.Unboxed (amap,assocs,(!),(!?))

main =
  do inp <- amap (read @Int . (:[])) <$> getInputArray 10
     print (part1 inp)
     print (part2 inp)

part1 m = sum paths
  where
    starts = [ c | (c,0) <- assocs m ]
    nexts c = [ d | d <- cardinal c, let x = m ! c, Just y <- [m !? d], x+1 == y ]
    paths = [ count ((9==) . (m !)) $ bfsOn id nexts [start] | start <- starts ]

part2 m = sum paths
  where
    starts = [ c | (c,0) <- assocs m ]
    nexts cs@(c:_) = [ (d:cs) | d <- cardinal c, let x = m ! c, Just y <- [m !? d], x+1 == y ]
    paths = [ count (\(c:_) -> m ! c == 9) (bfsOn id nexts [[start]]) | start <- starts ]
