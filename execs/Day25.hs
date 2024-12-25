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
  do inp <- getInput parse 25
     print (part1 inp)
     print (part2 inp)
  where
    parse (map lines . L.splitOn "\n\n" -> xss) = (map (pins . tail) ls,map (pins . init) ks)
      where
        (ls,ks) = L.partition (all ('#'==) . head) xss
        pins = map (count ('#'==)) . L.transpose

part1 (ls,ks) = length [ undefined | l <- ls, k <- ks, fit l k ]

fit l k = and [ 5-x >= y | x <- l | y <- k ]

part2 = const ()
