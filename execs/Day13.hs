module Main (main) where

import Advent (getInput)
import Data.Char (isDigit)
import Data.List.Split (splitOn)

main =
  do inp <- getInput parse 13
     print (part1 inp)
     print (part2 inp)
  where
    parse = map nums . splitOn "\n\n"
    nums = map (read @Int) . words . map \case c | isDigit c -> c; _ -> ' '

part1 = sum . map solve

solve [x1,y1,x2,y2,c1,c2]
  | x1*a+x2*b == c1 && y1*a+y2*b == c2 = 3*a+b
  | otherwise = 0
  where
    a = (c2*x2-c1*y2)`div`(x2*y1-x1*y2)
    b = (c2*x1-c1*y1)`div`(x1*y2-x2*y1)

part2 = part1 . map (\[x1,y1,x2,y2,c1,c2] -> [x1,y1,x2,y2,10_000_000_000_000+c1,10_000_000_000_000+c2])
