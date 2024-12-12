module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.List          qualified as L
import Data.List.Split    qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInputArray 12
     print (part1 inp)
     print (part2 inp)

part1 a = sum [ length area * perim area | css <- M.elems areas, area <- css ]
  where
    areas = go S.empty (A.indices a) M.empty
      where
        go _ [] m = m
        go seen (c:cs) m = go seen' cs' m'
          where
            x = a A.! c
            ds = bfs same c
            same d = [ e | e <- cardinal d, Just y <- [a A.!? e], y == x ]
            seen' = foldl' (flip S.insert) seen ds
            cs' = filter (`S.notMember` seen') cs
            m' = M.insertWith (\[new] old -> new : old) x [ds] m

    perim (S.fromList -> area) = count (`S.notMember` area) (concatMap cardinal area)

{-
    starts = M.fromListWith const [ (x,c) | (c,x) <- A.assocs a ]

    nexts x c = [ d | d <- neighbors c, Just y <- [a A.!? d], y == x ]
    repr = id
    areas = [ (x,bfsOn repr (nexts x) [c]) | (x,c) <- M.assocs starts ]

    perims = [ perim x area | (x,area) <- areas ]
    perim x (S.fromList -> area) = (x,count (`S.notMember` area) (concatMap cardinal area))
-}

-- wrong: 102868

part2 = const ()

nub' :: Ord a => [a] -> [a]
nub' = map head . L.group . L.sort

-- [C 3 3,C 2 3,C 2 2,C 1 2]
-- [C 0 2,C 1 1,C 1 2,C 1 3,C 2 1,C 2 2,C 2 3,C 2 4,C 3 2,C 3 3,C 3 4,C 4 3]
--   0123
-- 0 AAAA
-- 1 BBCD
-- 2 BBCC
-- 3 EEEC
