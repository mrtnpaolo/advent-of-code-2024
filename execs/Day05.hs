module Main (main) where

import Advent          (getInput)
import Data.List       (partition, intersect, sortBy)
import Data.List.Split (splitOn)

main =
  do [rs,xss] <- getInput (parse . map clean) 5
     let (good,bad) = partition (ordered rs) xss
     print (part1 good)
     print (part2 rs bad)
  where
    clean '|' = ' '; clean ',' = ' '; clean c = c
    parse (splitOn "\n\n" -> xs) = map (map (map (read @Int) . words) . lines) xs
    ordered rs xs = and [ ys == r | r <- rs, let ys = intersect xs r, length ys == 2 ]

part1 xss = sum [ mid xs | xs <- xss ] where mid xs = xs !! (length xs `div` 2)

part2 rs  = part1 . map (sortBy rules)
  where
    rules x y | [x,y] `elem` rs = LT | [y,x] `elem` rs = GT | otherwise = compare x y
