module Main (main) where

import Advent          (getInput,count)
import Data.List       (partition,transpose)
import Data.List.Split (splitOn)

main =
  do inp <- getInput (parse . map lines . splitOn "\n\n") 25
     print (part1 inp)
  where
    parse ss = (map (pins . tail) ls,map (pins . init) ks)
      where
        (ls,ks) = partition (all ('#'==) . head) ss
        pins = map (count ('#'==)) . transpose

part1 (ls,ks) = count fit [ (l,k) | l <- ls, k <- ks ]

fit (l,k) = and [ 5-x >= y | x <- l | y <- k ]
