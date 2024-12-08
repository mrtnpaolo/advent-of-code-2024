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
import Control.Monad
import Debug.Trace

main =
  do inp <- getInputMap 8
     print (part1 inp)
     print (part2 inp)

part1 m = length (S.fromList (concat yss))
  where
    xs = [ (c,x) | (c,x) <- M.assocs m, x /= '.' ]
    rs = M.fromListWith (++) [ (x,[c]) | (c,x) <- xs ]
    Just bs = boundingBox (M.keys m)
    inside = inRange bs
    yss =
      [ extend a b ++ extend b a
      | (_,cs) <- M.assocs rs
      , [a,b] <- replicateM 2 cs
      , a /= b
      ]
    extend a b = takeWhile inside [addCoord (scaleCoord 2 d) b]
      where
        d = addCoord a (scaleCoord (-1) b)

part2 m = length (S.fromList (concat yss))
  where
    xs = [ (c,x) | (c,x) <- M.assocs m, x /= '.' ]
    rs = M.fromListWith (++) [ (x,[c]) | (c,x) <- xs ]
    Just bs = boundingBox (M.keys m)
    inside = inRange bs
    yss =
      [ extend a b ++ extend b a
      | (_,cs) <- M.assocs rs
      , [a,b] <- replicateM 2 cs
      , a /= b
      ]
    extend a b = tail $ takeWhile inside (iterate (addCoord d) b)
      where
        d = addCoord a (scaleCoord (-1) b)
