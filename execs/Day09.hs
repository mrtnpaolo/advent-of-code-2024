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
  do inp <- getInput parse 9
     print (part1 inp)
     print (part2 inp)
  where
    parse xs = go zs
      where
        ys = f xs
        n = length ys
        zs = filter (not . null . snd) $ zip (L.intersperse (-1) [0..]) ys
        go [] = []
        go ((-1,length -> n):xs) = replicate n (-1) ++ go xs
        go (( m,length -> n):xs) = replicate n    m ++ go xs
    f = (\xs -> zipWith replicate xs [0..]) . map (read @Int) . init . map (:[])

{-
    parse xs = ()
      where
        (ys,zs) = go [] [] xs
    go a b [] = (reverse a,init (reverse b))
    go a b (x:y:xs) = go (x:a) (y:b) xs
-}

part1 = const ()
{-
part1 xs = checksum $ go [] xs
  where
    go ans xs
      | null rs'  = traceShow ("ls",ls) $ traceShow ("rs",rs) $ traceShow ("ds",ds) $ traceShow ("ss",ss) $ traceShow ("ns",ns) $ traceShow ("rs'",rs') $ (ans++ls++ss)
      | otherwise = go (ans++ls++ns) rs'
      where
        (ls,rs)  = L.span (>=0) xs
        (ds,ss)  = L.span (< 0) rs
        (ns,rs') = consume [] ds (reverse ss)
    consume ns [] ss        = (reverse ns,reverse ss)
    consume ns ds ((-1):ss) = consume ns ds ss
    consume ns ((-1):ds) (n:ss) = consume (n:ns) ds ss
    checksum = sum . zipWith (*) [1..] . tail
-}

-- wrong
-- 6215767103896
-- correct
-- 6216544403458

checksum = go [1..] . tail
  where
    go _      []        = 0
    go (_:ns) ((-1):xs) = go ns xs
    go (n:ns) (x:xs)    = n*x + go ns xs

part2 xs = checksum $ go ids xs
  where
    ids = reverse $ L.nub $ filter (>=0) xs
    go [] ys = ys
    go (k:ks) (L.groupBy (==) -> ys)
      | Just j <- L.findIndex (\xs@(x:_) -> x == -1 && length xs >= l) ls =
        let
          (ls',dst:rs') = L.splitAt j ls
          ys' = ls' ++ combine src dst : rs' ++ (take (length src) (repeat (-1)) : rs)
        in
          go ks (concat ys')
      | otherwise = go (ks) (concat ys)
      where
        Just i = L.findIndex ((k==) . head) ys
        (ls,src:rs) = L.splitAt i ys
        l   = length src
    combine [] dst = dst
    combine (s:src) (_:dst) = s : combine src dst

{-

part2 xs = checksum $ traceShowId $ go ids xs
  where
    ids = reverse $ L.nub $ filter (>=0) xs
    go [] ys = ys
    go (k:ks) (L.groupBy (==) -> ys)
      | Just j <- L.findIndex (\xs@(x:_) -> x == -1 && length xs >= l) ls =
        let
          (ls',dst:rs') = L.splitAt j ls
          ys' = traceShow (src,dst) $ ls' ++ combine src dst : rs' ++ (take (length src) (repeat (-1)) : rs)
        in
          traceShow ("fitting",k) $ go ks (concat ys')
      | otherwise = go (traceShow ("cant fit",k) ks) (concat ys)
      where
        Just i = L.findIndex (all (k==)) ys
        (ls,src:rs) = L.splitAt i ys
        l   = length src
    combine [] dst = dst
    combine (s:src) (_:dst) = s : combine src dst
-}
