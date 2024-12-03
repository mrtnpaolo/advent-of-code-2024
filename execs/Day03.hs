module Main (main) where

import Advent
import Data.Char

main =
  do inp <- getInput id 3
     print (part1 inp)
     print (part2 inp)

part1 = sum . map (uncurry (*)) . parse1

parse1 xs
  | ('m':'u':'l':'(':xs1) <- xs
  , (l@(_:_),',':xs2) <- (takeWhile isDigit xs1,dropWhile isDigit xs1)
  , (r@(_:_),')':xs3) <- (takeWhile isDigit xs2,dropWhile isDigit xs2)
  = (read @Int l,read @Int r) : parse1 xs3

parse1 (_:xs) = parse1 xs

parse1 _ = []

part2 = sum . map (uncurry (*)) . parse2 True

parse2 False xs | ('d':'o':'(':')':xs1) <- xs = parse2 True xs1

parse2 True xs | ('d':'o':'n':'\'':'t':'(':')':xs1) <- xs = parse2 False xs1

parse2 True xs
  | ('m':'u':'l':'(':xs1) <- xs
  , (l@(_:_),',':xs2) <- (takeWhile isDigit xs1,dropWhile isDigit xs1)
  , (r@(_:_),')':xs3) <- (takeWhile isDigit xs2,dropWhile isDigit xs2)
  = (read @Int l,read @Int r) : parse2 True xs3

parse2 e (_:xs) = parse2 e xs

parse2  _ _ = []
