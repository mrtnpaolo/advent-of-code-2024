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
  do inp <- getInputLines parse 14
     print (part1 inp)
     -- let m = part1 inp
     -- putStr (drawCoords $ fmap intToDigit $ M.fromListWith (+) [ (c,1) | (c,_) <- m ])

     let ((i,cs):_) = part2 inp
     print i
     putStr (drawCoords $ fmap intToDigit $ M.fromListWith (+) [ (c,1) | c <- cs ])

{-
     forM_ (part2 inp) $ \(i,cs) ->
       do putStr "ITERATION: "
          print i
          putStr (drawCoords $ fmap intToDigit $ M.fromListWith (+) [ (c,1) | c <- cs ])
          _ <- getChar
          pure ()
-}
     --print (part2 inp)
     --let m = (part2 imp)
     --putStr (drawCoords $ fmap intToDigit $ M.fromListWith (+) [ (c,1) | c <- cs ])
  where
    parse xs = (C py px,C vy vx)
      where
        [px,py,vx,vy] = map (read @Int) . words . map (\case c | c `elem` ('-':['0'..'9']) -> c; _ -> ' ') $ xs

part1 inp = q1*q2*q3*q4
  where
    Just (_,C yM xM) = boundingBox [ c | (c,_) <- inp ]
    step (C y x,v@(C vy vx)) = (C ((y+vy) `mod` (yM+1)) ((x+vx) `mod` (xM+1)),v)
    run n xs = iterate (map step) xs !! n
    end = run 100 inp
    (ymid,xmid) = (yM `div` 2, xM `div` 2)
    (q1,q2,q3,q4) = L.foldl' classify (0,0,0,0) [ c | (c,_) <- end ]
    classify (a,b,c,d) (C y x)
      | y < ymid && x < xmid = (succ a,b,c,d)
      | y < ymid && x > xmid = (a,succ b,c,d)
      | y > ymid && x < xmid = (a,b,succ c,d)
      | y > ymid && x > xmid = (a,b,c,succ d)
      | otherwise = (a,b,c,d)

-- 224357412 too high
part2 inp = filter (blob . snd) [ (i,cs) | i <- [0..] | cs <- map (map fst) run ]
  where
    Just (_,C yM xM) = boundingBox [ c | (c,_) <- inp ]
    -- Just (inRange -> inside) = boundingBox [ c | (c,_) <- inp ]
    step (C y x,v@(C vy vx)) = (C ((y+vy) `mod` (yM+1)) ((x+vx) `mod` (xM+1)),v)
    run = iterate (map step) inp
    center = L.sort $ bookreading (C (yM`div`2) (xM`div`2))
    blob cs@(S.fromList -> cs') = any (\c -> S.fromList (bookreading c) `S.isSubsetOf` cs') cs
{-
    blob cs = ys < 90 || xs < 90
      where
        Just (C ym xm,C yM xM) = boundingBox cs
        ys = yM-ym
        xs = xM-xm
-}
{-
    corners (pred -> n) = S.fromList $ concat
      -- [ range (origin,origin + C n n)
      -- , range (C 0 (xM-n),C yM n)
      [ range (C (yM-n) 0,C yM n)
      , range (C (yM-n) (xM-n),C yM xM) ]
    crnrs = corners 8
    empty = (`S.notMember` crnrs)
    blob = all empty
-}
{-
    xmid = xM `div` 2
    v = S.fromList [ C y xmid | y <- [0..yM] ]
    goal = S.size v `div` 8
    blob (S.fromList -> cs) = S.size (cs `S.intersection` v) >= goal
-}
    -- blob cs@(S.fromList -> cs') = all (any (`S.member` cs') . neighbors) cs
{-
    n = length inp
    blob (c:(S.fromList -> cs)) = n == length (bfs next c)
      where
        next c = [ d | d <- neighbors c, inside d, S.member d cs ]
-}
