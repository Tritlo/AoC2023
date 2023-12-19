{-# LANGUAGE  GHC2021 #-}
module Main where

import qualified Data.Array as Arr
import Data.Array (Array)

import Data.Set (Set)
import qualified Data.Set as Set

import Prelude hiding (Left, Right)

data Dir = Up | Down | Left | Right
  deriving (Eq, Ord)

instance Show Dir where
    show Up = "^"
    show Down = "v"
    show Left = "<"
    show Right = ">"



parse :: [String] -> Array (Int,Int) Int
parse strs = Arr.array ((1,1), fst (last res)) res
  where parseEntry c = read @Int [c]
        f = concat . zipWith (\i ls -> map (\(j,e) -> ((i,j),e)) ls) [1..] . map (zip [1..] . map parseEntry)
        res = f strs

-- Lessons learned:
-- + Don't try to track the path! It is too expensive
-- + Sets are heaps! And way faster than a linked list.
-- + Track the direction we came from, that is the only
--   thing that can change the result. No need to track
--   the path!
-- + Always turn, if we wanted to continue going straight,
--   it would have been covered by a previous entry.
-- + We had the right idea! We were just missing the
--   "take multiple steps" and tracking only the direction
--   part.


dijkstra2D :: Int -> Int -> Array (Int,Int) Int -> Int
dijkstra2D min_step max_step arr =
    let inBounds (y,x) = y >= i_y && x >= i_x &&
                         y <= t_y && x <= t_x
        pf !cc [c@(p,_)] = (cc + arr Arr.! p, c)
        pf !cc ((p,_):cs) = pf (cc + (arr Arr.! p)) cs
        go :: Set ((Int,Int),Dir) -> Set (Int,((Int,Int),Dir)) -> Int
        -- Sets are heaps in Haskell, except with no repitition.
        go !visited !heap_queue
          | pos == target = pl
          | (pos,dir) `Set.member` visited = go visited rest
          | otherwise =
              let visited' = Set.insert (pos,dir) visited
                  -- only turn, straight would have been covered if allowed.
                  v_dirs = if dir == Left || dir == Right
                           then [Up,Down] else [Left,Right]

                  steps = v_dirs >>= (\d -> map (flip replicate d) [min_step..max_step])
                  paths = map (moves pos) steps
                  valid_paths = filter (all (inBounds . fst)) paths
                  p_pls = Set.fromList $ map (pf pl) valid_paths

              in go visited' $ Set.union p_pls heap_queue
          where  ((pl, (pos,dir)),rest) = Set.deleteFindMin heap_queue


    in (go Set.empty $ Set.fromList $ [(0,(init,Right)), (0,(init,Down))])
  where (init@(i_y,i_x),target@(t_y,t_x)) = Arr.bounds arr
        move :: (Int,Int) -> Dir -> (Int,Int)
        move (y,x) Left = (y, x-1)
        move (y,x) Right = (y, x+1)
        move (y,x) Up = (y-1, x)
        move (y,x) Down = (y+1, x)
        moves :: (Int,Int) -> [Dir] -> [((Int,Int),Dir)]
        moves c [] = []
        moves c (d:ds) = (c',d):(moves c' ds)
          where c' = move c d



main :: IO ()
main = do readFile "example" >>= print .  dijkstra2D 1 3. parse . lines
          readFile "input" >>= print .  dijkstra2D 1 3. parse . lines
          readFile "example" >>= print .  dijkstra2D 4 10. parse . lines
          readFile "input" >>= print .  dijkstra2D 4 10. parse . lines
