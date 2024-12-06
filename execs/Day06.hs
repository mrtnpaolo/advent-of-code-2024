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
  do m <- getInputMap 6
     let [g] = [ c | c <- M.keys m, m M.! c == '^' ]
         m'  = M.insert g '.' m
     -- print (length [ () | c <- M.keys m', m' M.! c == '.' ])
     print (part1 g m')
     print (part2 g m')

part1 g m = 1 + (length $ S.fromList $ go north g)
  where
    Just bs = boundingBox (M.keys m)
    inside  = inRange bs

    go dir c
      | not (inside c) = []
      | otherwise =
        case m M.!? (step dir c) of
          Nothing -> []
          Just '#' -> go (turnRight dir) c
          Just _   -> c : go dir (step dir c)

    step N = addCoord north
    step E = addCoord east
    step S = addCoord south
    step W = addCoord west

part2 g m = length xs
  where
    xs =
      [ c
      | c <- cs
      , c /= g
      , let x = m M.! c
      , x == '.'
      , let m'   = M.insert c '#' m
      , let path = go m' north g
      --, ouroboros path ]
      , not $ null $ drop (n) path ]

    cs = range bs

    n = count ('.'==) [ m M.! c | c <- cs ]

{-
    ouroboros (x:xs) = f (S.singleton x) xs
      where
        f seen (x:xs) | S.member x seen = True | otherwise = f (S.insert x seen) xs
        f _ [] = False
-}

    Just bs = boundingBox (M.keys m)
    inside  = inRange bs

    go m1 dir c
      | not (inside (step dir c)) = []
      | otherwise =
        case m1 M.! (step dir c) of
          '#' ->     go m1 (turnRight dir) c
          _   -> c : go m1 dir (step dir c)

    step N = addCoord north
    step E = addCoord east
    step S = addCoord south
    step W = addCoord west


