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
  do inp <- getInput parse 5
     print (part1 inp)
     print (part2 inp)
  where
    parse (L.splitOn "\n\n" -> [a,b]) = (ps,ns)
      where
        ps = [ let [x,y] = map (read @Int) (L.splitOn "|" l) in (x,y) | l <- lines a ]
        ns = [ map (read @Int) (L.splitOn "," l) | l <- lines b ]

ordered (x,y) xs
  | Just xi <- L.findIndex (x==) xs
  , Just yi <- L.findIndex (y==) xs
  = xi < yi
  | otherwise = True

part1 (ps,nss) = sum [ mid xs | xs <- nss, and [ ordered p xs | p <- ps ] ]

mid xs = xs !! (length xs `div` 2)

part2 (ps,nss) = sum [ mid $ reorder ps xs | xs <- nss, or [ not (ordered p xs) | p <- ps ] ]

reorder ps = L.sortBy f
  where
    f x y
      | (x,y) `elem` ps = LT
      | (y,x) `elem` ps = GT
      | otherwise       = compare x y

