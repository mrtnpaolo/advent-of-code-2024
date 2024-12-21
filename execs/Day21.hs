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

-- 029A
-- 980A
-- 179A
-- 456A
-- 379A

main =
  do inp <- getInputLines parse 21
     -- dbg
     print (part1 inp)
     print (part2 inp)
  where
    parse xs = (n,xs) :: (Int,String) where [(n,_)] = reads xs

k1, k2 :: A.UArray Coord Char
k1 = A.listArray (C 0 0,C 3 2) $ concat
  [ "789"
  , "456"
  , "123"
  , " 0A" ]
k2 = A.listArray (C 0 0,C 1 2) $ concat
  [ " ^A"
  , "<v>" ]

sp k = [ (a,b,go a b) | (a,b) <- ps ]
  where
    ps = [ ((c,x),(c',x')) | (c,x) <- cs, x /= ' ', (c',x') <- cs, x' /= ' ' ] where cs = A.assocs k
    starts = M.fromList [ (x,c) | (c,x) <- A.assocs k, x /= ' ' ]
    go (_,a) (_,b) = xss
      where
        paths@(one:_) = [ reverse path | path@(c:_) <- bfsOn S.fromList next [[starts M.! a]], k A.! c == b ]
        n = length one
        xss = takeWhile ((<=n) . length) paths
    next cs@(c:_) = [ c':cs | c' <- cardinal c, inside c', k A.! c' /= ' ' ]
    Just (inRange -> inside) = boundingBox (A.indices k)

f a b =
  case b-a of
    N -> '^'
    W -> '<'
    S -> 'v'
    E -> '>'

g (a:b:cs) = f a b : g (b:cs)
g _ = []

dbg =
  do putStrLn "SP1"
     mapM_ print [ (a,b,map g cs) | ((_,a),(_,b),cs) <- sp k1 ]
     putStrLn "SP2"
     mapM_ print [ (a,b,map g cs) | ((_,a),(_,b),cs) <- sp k2 ]
     -- print sp1
     -- mapM_ print $ M.toList sp2

--part1 inp = [ (n,length keys - 2) | (n,goal) <- inp, let keys = solve sp2 . solve sp2 . solve sp1 $ goal ]

part1 inp = sum
  [ n * length keys
  | (n,goal) <- inp
  , let keys = L.minimumBy (comparing length) $ concatMap (solve sp2) $ concatMap (solve sp2) $ solve sp1 goal ]

{-
part1 inp =
  [ (n,length keys)
  | (n,goals) <- inp
  , goal <- goals
  , let keys = L.minimumBy (comparing length) $
                 [ solve sp2 k2 | k1 <- solve sp1 goal, k2 <- solve sp2 k1 ]
  ]
-}

sp1 = M.fromList [ ((a,b),map g cs) | ((_,a),(_,b),cs) <- sp k1 ]
sp2 = M.fromList [ ((a,b),map g cs) | ((_,a),(_,b),cs) <- sp k2 ]

{-
solve ps = inputs
  where
    inputs = traceShowId . concatMap (++"A") . go . ('A':)
    go (a:b:cs) = ps M.! (a,b) : go (b:cs)
    go [_] = []
-}

solve ps = inputs
  where
    inputs = {- traceShowId . -} {- concatMap (++"A") . -} go . ('A':)
    go (a:cs@(b:_)) = [ opt ++ 'A' : end | opt :: String <- opts, end <- rest ]
      where
        opts = ps M.! (a,b)
        rest = go cs
    go [_] = [[]]


part2 = const ()
