module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.List.Split    qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInput parse 24
     print (part1 inp)
     print (part2 inp)
  where
    parse (L.splitOn "\n\n" -> [xss,yss]) = (vs,fs)
      where
        vs :: M.Map String Bool
        vs = M.fromList [ (x,fv v) | [x,v] <- map (words . map \case ':' -> ' '; c -> c) (lines xss) ]
        fv = ("1"==)
        fs :: M.Map String (String,String,Bool->Bool->Bool)
        fs = M.fromList [ ff v | v <- map (words . map \case c | c `elem` "->" -> ' '; c -> c) (lines yss) ]
        ff [x,"AND",y,z] = (z,(x,y,(&&)))
        ff [x,"OR", y,z] = (z,(x,y,(||)))
        ff [x,"XOR",y,z] = (z,(x,y,(/=)))

part1 (vs,fs) = sum n
  where
    ns = L.sort [ v | v@('z':_) <- M.keys vs ] ++ (filter (('z'==) . head) $ concat [ [x,y,z] | (z,(x,y,_)) <- M.assocs fs ])
    vs' = go fs vs ns
    n = [ (if v then 1 else 0) * 2^i | (_,v) <- M.toAscList (M.restrictKeys vs' (S.fromList ns)) | i <- [0..] ]

go _  vs [] = vs
go fs vs (g:gs)
  | g `M.member` vs = go fs vs gs
  | (x,y,f) <- fs M.! g, Just xv <- vs M.!? x, Just yv <- vs M.!? y = go fs (M.insert g (f xv yv) vs) gs
  | (x,y,_) <- fs M.! g = go fs vs (x:y:g:gs)

-- too low: 20146511324955

part2 = const ()
