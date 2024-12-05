module Main (main) where

import Advent          (getInputMap, count)
import Advent.Coord    (boundingBox, right, below, left, above)
import Data.Ix         (range, inRange)
import Data.Map.Strict (keys, (!))

main =
  do inp <- getInputMap 4
     print (part1 inp)
     print (part2 inp)

part1 m = count xmas (coords >>= word)
  where
    Just bounds@(range -> coords) = boundingBox (keys m)
    look    = [ right, below, below . right, above . right ]
    word c  = [ cs | d <- look, let cs = take 4 (iterate d c), all (inRange bounds) cs ]
    xmas cs = [ m ! c | c <- cs ] `elem` [ "XMAS", "SAMX" ]

part2 m = count cross (range (cm,cM))
  where
    Just (right . below -> cm,left . above -> cM) = boundingBox (keys m)
    look    = [ [ left . above, id, right . below ], [ left . below, id, right . above ] ]
    cross c = all (`elem` [ "MAS", "SAM" ]) [ [ m ! d c | d <- ds ] | ds <- look ]
