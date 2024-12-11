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
import Data.Sequence      (Seq((:<|),(:|>)),(><))
import Data.Sequence      qualified as Seq
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Control.Applicative
import Debug.Trace

main =
  do inp <- getInput parse 11
     print (part1 inp)
     print (part2 inp)
  where
    parse = map (read @Int) . words

part1 = const ()
{-
part1 xs = Seq.length $ iterate (Seq.foldMapWithIndex step) xs !! 75
-- foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Seq a -> m
step _ 0 = Seq.singleton 1
step _ n
  | xs <- show n, l <- length xs, even l, k <- l `div` 2 = read @Int (take k xs) :<| read @Int (drop k xs) :<| Seq.empty
  | otherwise = Seq.singleton (n * 2024)
-}

{-
part2 xs = sum [ mul | (_,mul) <- muls ]
  where
    muls = go 75 [ (x,1) | x <- xs ]

go 0 xs = xs
go i xs = go (pred i) xs'
  where
    xs' = [ (y,mul * length ys) | ys@((y,mul):_) <- L.group (L.sortOn fst [ (x,mul) | (n,mul) <- xs, x <- step n ]) ]
-}

{-
step 0 = [1]
step n
  | xs <- show n, l <- length xs, even l, k <- l `div` 2 = [read @Int (take k xs),read @Int (drop k xs)]
  | otherwise = [n * 2024]

part2 xs = go 25 0 a xs'
  where
    m = maximum xs * 2024
    a :: A.UArray (Int,Int) Int
    a = A.genArray ((0,0),(m,75)) (\case (n,0) | elem n xs' -> 1; _ -> 0)
    xs' = map head $ L.group $ L.sort xs

-- go k 0 a xs = [ a A.! (x,k) | x <- xs ]
go k i a xs
  | k == i = [ a A.! (x,k) | x <- xs ]
  | otherwise = go k i' a' [ n | (n,_) <- xs' ]
  where
    i' = succ i
    xs' = [ (n,length ns) | ns@(n:_) <- L.group $ L.sort $ concat $ map step xs ]
    ys = [ ((n,i'),if prev == 0 then mul else (prev * mul)) | (n,mul) <- xs', let prev = a A.! (n,i) ]
    a' = a A.// ys
-}

step 0 = [1]
step n
  | xs <- show n, l <- length xs, even l, k <- l `div` 2 = [read @Int (take k xs),read @Int (drop k xs)]
  | otherwise = [n * 2024]

part2 xs = go 75 0 a [ n | ((n,0),_) <- xs' ]
  where
    xs' = [ ((n,0),length ns) | ns@(n:_) <- L.group $ L.sort xs ]
    a = M.fromList xs'

go k i a xs
  -- | all (`M.member` a) [ (x,k) | x <- xs ] = sum [ a M.! (x,k) | x <- xs ]
  | i == k = sum [ a M.! (x,k) | x <- xs ] -- [ (x,a M.! (x,k)) | x <- xs ]
  | otherwise = go k i' a' (map head $ L.group $ L.sort [ x | ((x,_),_) <- xs' ])
  where
    i' = succ i
    xs' = [ ((n,i'),mul) | x <- xs, let mul = a M.! (x,i), n <- step x ]
    a' = foldl' (\m (k,v) -> M.insertWith (+) k v m) a xs'

{-
    xs' = [ (n,length ns) | ns@(n:_) <- L.group $ L.sort $ concat $ map step xs ]
    a' = foldl' (\m (k,v) -> M.insert k v m) a
      [ ((x,i'),n)
      | (x,mul) <- xs'
      , Just prev <- [ a M.!? (x,i) <|> Just 1 ]
      , let n = mul * prev ]
-}

{-
go k i a xs
  | k == i = [ (x,a M.! (x,k)) | x <- xs ]
  | otherwise = go k i' a' [ n | (n,_) <- xs' ]
  where
    i' = succ i
    xs' = [ (n,length ns) | ns@(n:_) <- L.group $ L.sort $ concat $ map step xs ]
    ys = [ ((n,i'),if prev == 0 then mul else (prev * mul)) | (n,mul) <- xs', Just prev <- [a M.!? (n,i) <|> Just 0] ]
    a' = foldl' (\m (k,v) -> M.insert k v m) a ys
-}
