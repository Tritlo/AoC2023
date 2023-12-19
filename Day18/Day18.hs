{-# LANGUAGE GHC2021 #-}
module Main where

import Numeric (readHex)

data Dir =  North
          | South
          | East
          | West
  deriving (Eq, Show, Ord, Enum)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]

move :: (Int,Int) -> Dir -> (Int,Int)
move (y,x) West = (y, x-1)
move (y,x) East = (y, x+1)
move (y,x) North = (y-1, x)
move (y,x) South = (y+1, x)
moves :: (Int,Int) -> [Dir] -> [((Int,Int),Dir)]
moves c [] = []
moves c (d:ds) = (c',d):(moves c' ds)
  where c' = move c d

parse :: [String] -> [(Dir, Int)]
parse = map parse'
  where parse' str | [d,n,_] <- splitOn ' ' str = (parseDir d, read @Int n)
        parseDir "R" = East
        parseDir "L" = West
        parseDir "D" = South
        parseDir "U" = North

parse2 :: [String] -> [(Dir, Int)]
parse2 = map parse'
  where parse' str | [_,_,'(':'#':s] <- splitOn ' ' str,
                     hexDigs <- take 5 s,
                     [(hex_num, "")] <- readHex hexDigs,
                     dirDig <- take 1 $ drop 5 s
                     = (parseDir dirDig, hex_num)
        parseDir "0" = East
        parseDir "2" = West
        parseDir "1" = South
        parseDir "3" = North


from (y1, x1) (y2, x2) | y1 == y2 = if x1 < x2
                                    then East
                                    else West
                       | x1 == x2 = if y1 < y2
                                    then North
                                    else South

part1 :: [(Dir, Int)] -> Int
part1 p = (greenArea (fst $ head mp) 0 mp)
     -- (greenArea 0 $ moves (0,0) path_p) `div` 4
   where path_p = path p
         mp = moves (0,0) path_p
         path [] = []
         path ((d,n):ds) = (replicate n d) <> (path ds)
         -- Sketch for speed:
         -- just calculate joints, [East,North,West],
         -- make sure to increment the y,
         -- East 6 = East 5 ++ join point
         -- East n would contribute ((n-1)*(-4*y +2)) etc.
         delta y [d1,d2] = case  [d1,d2] of
                             [East, East]   -> -4*y +2
                             [East, North]  -> -2*y +1
                             [East, South]  -> -2*y +3
                             [West, West]   -> 4*y +2
                             [West, South]  -> 2*y +1
                             [West, North]  -> 2*y +3
                             [North, North] -> 2
                             [North, East]  -> -2*y +3
                             [North, West]  -> 2*y +1
                             [South, South] -> 2
                             [South, East]  -> -2*y + 1
                             [South, West]  -> 2*y + 3
         greenArea _ !gsf [] = gsf `div` 4
         greenArea orig !gsf [(x@(y,_),d1)] =
            greenArea orig (gsf + (delta y [d1,d2])) []
                where d2 = from x orig
         greenArea orig !gsf (((y,_),d1):(x@(_,d2)):ys)
             =  greenArea orig (gsf + (delta y [d1,d2])) (x:ys)


main :: IO ()
main = do readFile "example" >>= print . part1 . parse . lines
          readFile "input" >>= print . part1 . parse . lines
          readFile "example" >>= print . part1 . parse2 . lines
          readFile "input" >>= print . part1 . parse2 . lines
