module Main (main) where

import Advent
import Data.List          qualified as L (partition)
import Data.List.Split    qualified as L (splitOn)
import Data.Array.Unboxed qualified as A (UArray, listArray,assocs, (!), (//), bounds, elems)

type Maze = A.UArray Coord Char

main =
  do inp <- getInput parse 15
     print (part1 inp)
     print (part2 inp)
  where
    parse xss = (m,dirs)
      where
        [ms,xs] = L.splitOn "\n\n" xss
        rows    = lines ms; height = length rows; width = length (head rows)
        bounds  = (origin, C (height-1) (width-1))
        m       = A.listArray bounds (concat rows) :: Maze
        dirs    = concat (lines xs)

part1 = gps 'O' . run

gps target m = sum [ 100*y + x | (C y x,c) <- A.assocs m, c == target ]

run (m,dirs) = go m start dirs
  where
    [start] = [ c | (c,'@') <- A.assocs m ]

    go m _ []     = m
    go m c (d:ds)
      | blocked   = go m  c  ds
      | otherwise = go m' c' ds
      where
        next = move d

        -- the items affected by a push in the direction d starting from c
        items = go (m A.! c) c
          where
            go '.' _            = "."
            go '#' _            = "#"
            go  x  (next -> c') =  x : go (m A.! c') c'

        blocked = '#' == last items

        m' = m A.//
          [ (c,x) | c <- iterate next c     -- ray from c in the direction d
                  | x <- '.':'@':repeat 'O' -- next over the robot once over
                  | _ <- items              -- only affect (length xs) spots
                  ]

        c' = next c

move '^' = above
move '>' = right
move 'v' = below
move '<' = left

part2 (m,ds) = gps '[' $ run2 (expand m) ds

expand m = A.listArray bs xs :: Maze
  where
    bs = (cm,C yM (2*xM+1)) where (cm,C yM xM) = A.bounds m
    xs = (\case '.'->"..";'#'->"##";'O'->"[]";'@'->"@.") `concatMap` (A.elems m)

run2 m ds = go m start ds
  where
    [start] = [ c | (c,'@') <- A.assocs m ]

    go m _ []     = m
    go m c (d:ds)
      | blocked   = go m  c  ds
      | otherwise = go m' c' ds
      where
        at = (m A.!)

        next = move d

        (ends,items) = dango m d c

        blocked = any (('#' ==) . at) ends

        m' = m A.// updates

        updates = [ (ci,     '.'  ) | ci <- items ] ++
                  [ (next ci,at ci) | ci <- items ] ++
                  [ (c,'.'), (c','@') ]

        c' = next c

dango :: Maze
      -> Char              {- direction     -}
      -> Coord             {- robot         -}
      -> ([Coord],[Coord]) {- (ends,blocks) -}

dango m d start = L.partition isEnd $ bfs (go d) (next start)
  where
    at = (m A.!)

    next = move d
    
    isEnd (at -> x) = x `elem` ".#"

    go  _    (at -> '.') = []
    go  _    (at -> '#') = []

    go '^' c@(at -> '[') = [right c, above c]
    go '^' c@(at -> ']') = [above c, left c ]

    go '>' c             = [right c]
 
    go 'v' c@(at -> '[') = [right c, below c]
    go 'v' c@(at -> ']') = [below c, left c ]

    go '<' c             = [left c]
