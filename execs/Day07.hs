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
  do inp <- getInputLines parse 7
     print (part1 inp)
     print (part2 inp)
  where
    parse = map (read @Int) . words . map \case c | c `elem` ":" -> ' '; c -> c

part1 xss = sum [ x | (x:xs) <- xss, any (x ==) (f (reverse xs)) ]

f [x] = [x]
f (x:xs) = map (x +) ys ++ map (x *) ys where ys = f xs

part2 xss = sum [ x | (x:xs) <- xss, any (x ==) (g (reverse xs)) ]

g [x] = [x]
g (x:xs) = map (x+) ys ++ map (x*) ys ++ map (h x) ys
  where
    ys = g xs
    h x y = read @Int (show y ++ show x)
