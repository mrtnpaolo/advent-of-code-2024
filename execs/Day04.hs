module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.List.Split    qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInputMap 4
     print (boundingBox (M.keys inp))
     print (part1 inp)
     print (part2 inp)

part1 m = length
  [ cs
  | c <- range b
  , cs <- ([h,h',v,v',d1,d1',d2,d2'] <*> [c])
  , inside cs
  , "XMAS" == map (m M.!) cs
  ]
  where
    Just b = boundingBox (M.keys m)
    h  c = take 4 (iterate right c)
    h' c = take 4 (iterate left  c)
    v  c = take 4 (iterate below c)
    v' c = take 4 (iterate above c)
    d1  c = take 4 (iterate (below . right) c)
    d1' c = take 4 (iterate (above . left ) c)
    d2  c = take 4 (iterate (above . right) c)
    d2' c = take 4 (iterate (below . left ) c)
    inside :: [Coord] -> Bool
    inside = all (inRange b)

part2 m = length
  [ ()
  | c <- range b
  , 'A' == m M.! c
  , let c1 = above (left c); c2 = above (right c); c3 = below (left c); c4 = below (right c)
  , let cs = [c1,c2,c3,c4]
  , all inside cs
  , let xs = map (m M.!) cs
  , xs `elem` ["MSMS","SSMM","MMSS","SMSM"]
  ]
  where
    Just b = boundingBox (M.keys m)
    inside = inRange b
