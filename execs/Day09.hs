module Main (main) where

import Prelude hiding (replicate)
import Advent         (getInput)
import Data.Char      (digitToInt)
import Data.List      (intersperse)
import Data.Foldable  (foldl',toList)
import Data.Sequence  (Seq((:<|),(:|>)),(><),fromList,dropWhileR,spanl,breakl,replicate,empty)

main =
  do inp <- getInput parse 9
     print (part1 inp)
     print (part2 inp)
  where
    parse xs = fromList
      [ (x,l) | (x,l) <- [ (x,digitToInt l) | l <- init xs | x <- intersperse (-1) [0..] ], l > 0 ]

part1 = checksum . go . dropWhileR (<0) . expand
  where
    go xs
      | (ls,ys) <- spanl (>=0) xs, -1 :<| zs <- ys, rs :|> n <- zs = ls >< n :<| go (dropWhileR (<0) rs)
      | otherwise = xs

part2 xs = checksum . expand . go ns $ xs
  where
    ns | _ :|> (x,_) <- xs = [x,x-1..1]
    go [] xs = xs
    go (n:ns) xs
      | (ls, a@(_,l) :<| rs ) <- breakl (\(x,_ ) ->  n==x         ) xs
      , (ls',(-1,l') :<| rs') <- breakl (\(y,l') -> -1==y && l'>=l) ls
      , rs'' <- if l == l' then rs' else (-1,l'-l) :<| rs'
      = go ns (ls' >< a :<| rs'' >< (-1,l) :<| rs)
      | otherwise = go ns xs

expand = foldl' (\xs (x,l) -> xs >< replicate l x) empty

checksum (toList -> xs) = sum [ n*x | (n,x) <- zip [1..] (tail xs), x >= 0 ]
