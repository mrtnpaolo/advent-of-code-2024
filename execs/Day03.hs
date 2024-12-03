module Main (main) where

import Advent    (getInput)
import Data.Char (isDigit)
import Data.List (splitAt)

main =
  do inp <- getInput id 3
     print (part1 inp)
     print (part2 inp)

part1 = sum . map (uncurry (*)) . parse

parse xs
  | ("mul(",xs1) <- splitAt 4 xs
  , l <- takeWhile isDigit xs1, ll <- length l, ll `elem` [1,2,3], xs2 <- drop ll xs1
  , (',':xs3) <- xs2
  , r <- takeWhile isDigit xs3, rl <- length r, rl `elem` [1,2,3], xs4 <- drop rl xs3
  , (')':xs5) <- xs4
  = (read @Int l,read @Int r) : parse xs5
parse (_:xs) = parse xs
parse []     = []

part2 = part1 . clean True

clean True  xs | ("don't()",rest) <- splitAt 7 xs = clean False rest
clean False xs | ("do()"   ,rest) <- splitAt 4 xs = clean True  rest
clean True  (x:xs) = x : clean True  xs
clean False (_:xs) =     clean False xs
clean _     []     = []
