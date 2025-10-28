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

p m cs =
  do putStrLn $ drawCoords $ M.fromList $ A.assocs m ++ [ (c,draw d) | ((c,d),_) <- cs ]
     print (L.find (\((c,_),_) -> c == C 1 139) cs)
     print [ (c,d,cost) | ((c,d),cost) <- cs, c == C 1 139 ]
  where
    draw N = '^'; draw E = '>'; draw S = 'v'; draw W = '<'

main =
  do inp <- getInputArray 16
     print (part1 inp)
     print (part2 inp)

part1 m = cost
  where
    Just (_,cost) = L.find (\((c,_),_) -> c == end) path

    [start] = [ c | (c,'S') <- A.assocs m ]
    [end]   = [ c | (c,'E') <- A.assocs m ]

    path = astar next (start,E)

    next (c,d)
      | c == end  = []
      | otherwise = [ AStep (c',d') cost 0 | (c',d',cost) <- options c d ]

    options c d = catMaybes [ walk c d c' d' | c' <- cardinal c | d' <- [N,E,S,W] ]

    walk c d c' d'
      | m A.! c' `elem` ".E" = Just (c',d',cost)
      | otherwise            = Nothing
      where
        turns | Just n <- L.elemIndex d' (iterate turnLeft d) = min n (4 - n)
        cost = manhattan c c' + turns * 1000

part2 = const ()
