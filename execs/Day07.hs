module Main (main) where

import Advent (getInputLines)

main =
  do inp <- getInputLines parse 7
     print (part1 inp)
     print (part2 inp)
  where
    parse = map (read @Int) . words . map \case ':' -> ' '; c -> c

part1 xss = sum [ x | (x:xs) <- xss, any (x ==) (f (reverse xs)) ]

f [x] = [x]
f (x:xs) = map (x +) ys ++ map (x *) ys where ys = f xs

part2 xss = sum [ x | (x:xs) <- xss, any (x ==) (g (reverse xs)) ]

g [x] = [x]
g (x:xs) = map (x+) ys ++ map (x*) ys ++ map (h x) ys
  where
    ys = g xs
    h x y = read @Int (show y ++ show x)
