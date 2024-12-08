module Main (main) where

import Advent             (getInputArray,pairs,boundingBox)
import Data.Ix            (inRange)
import Data.Set           (fromList)
import Data.Map.Strict    (fromListWith,elems)
import Data.Array.Unboxed (indices,assocs)

main =
  do m <- getInputArray 8
     let Just (inRange -> inside) = boundingBox (indices m)
         xs = fromListWith (++) [ (x,[c]) | (c,x) <- assocs m, x /= '.' ]
     print (part1 inside xs)
     print (part2 inside xs)

part1 inside xs = length $ fromList [ c | cs <- elems xs, (a,b) <- pairs cs, c <- [b+(b-a),a+(a-b)], inside c ]

part2 inside xs = length $ fromList [ c | cs <- elems xs, (a,b) <- pairs cs, c <- extend a b ++ extend b a ]
  where
    extend a b = tail $ takeWhile inside (iterate (d+) b) where d = a-b
