{-# LANGUAGE GHC2021 #-}
module Main where


import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace

data Dir = North
          | South
          | East
          | West
  deriving (Eq, Show, Ord, Enum)

type Pipe = [Dir]

data MapEntry = Ground
              | Start
              | Pipe Pipe
  deriving (Eq, Show, Ord)

parsePipe :: Char -> Pipe
parsePipe '|' = [North, South]
parsePipe 'L' = [North, East]
parsePipe 'F' = [South, East]
parsePipe '-' = [East, West]
parsePipe 'J' = [North, West]
parsePipe '7' = [South, West]

pipeType :: Pipe -> Char
pipeType [North, South] = '|'
pipeType [East,   West] = '-'
pipeType [North,  East] = 'L'
pipeType [North,  West] = 'J'
pipeType [South,  West] = '7'
pipeType [South,  East] = 'F'
pipeType [] = 'S'


parseMap :: Char -> MapEntry
parseMap '.' = Ground
parseMap 'S' = Start
parseMap c = Pipe (parsePipe c)

neigh :: (Int,Int) -> Dir -> (Int,Int)
neigh (x,y) East  = (x,y+1) -- East
neigh (x,y) West  = (x,y-1) -- West
neigh (x,y) North = (x-1,y) -- North
neigh (x,y) South = (x+1,y) -- South

corresp :: Dir -> Dir
corresp East = West
corresp West = East
corresp North = South
corresp South = North


neighs :: (Int, Int) -> [(Int, Int)]
neighs (x,y) = map (neigh (x,y)) [West, South, East, North, South]

--part1 :: [String] -> --(Map (Int,Int) MapEntry, (Int,Int))
part1 strs = (1 + go coords Set.empty) `div` 2
  where mp = (Map.fromList . concat . zipWith (\i l -> map (\(j,e) -> ((i,j), e)) l ) [0..] . map (zip [0..] . map parseMap)) strs
        ((coords,Start):[]) = Map.toList $ Map.filter (== Start) mp
        start_ns = neighs coords
        n_in_map c = case mp Map.!? c of
                        Just Start -> mapMaybe f [West, South, East, North]
                        Just (Pipe xs) -> mapMaybe f xs
                        _ -> []

           where f d = let nc = neigh c d
                       in case mp Map.!? nc of
                         Just (Pipe xs) | corresp d `elem` xs -> Just nc
                         _ -> Nothing
        go c !seen = case ns of
                      [] -> Set.size seen
                      (n:_) -> go n (Set.insert c seen)

          where ns = filter (not . flip Set.member seen) $ n_in_map c


part2 strs = case greenArea curve_1 of
                ga | ga >= 0 -> ga - curveArea curve_1
                ga -> abs ga - curveArea curve_2 -- we went the wrong way around.
  where mp = (Map.fromList . concat . zipWith (\i l -> map (\(j,e) -> ((i,j), e)) l ) [0..] . map (zip [0..] . map parseMap)) strs
        ((coords,Start):[]) = Map.toList $ Map.filter (== Start) mp
        pipe_types = Map.fromList $ map (\p -> (Set.fromList (parsePipe p), p)) "|LF-J7"
        s_type = pipe_types Map.! (Set.fromList (map (fst .fst) $ n_in_map coords))

        n_in_map :: (Int,Int) -> [((Dir, [Dir]),(Int,Int))]
        n_in_map c = case mp Map.!? c of
                        Just Start -> mapMaybe f [West, South, East, North]
                        Just (Pipe xs) -> mapMaybe f xs
                        _ -> []
           where f d = let nc = neigh c d
                       in case mp Map.!? nc of
                         Just (Pipe xs) | corresp d `elem` xs -> Just ((d,xs), nc)
                         Just Start -> Just ((d,parsePipe s_type), nc)
                         _ -> Nothing
        [s_n_1,s_n_2] = n_in_map coords
        -- We can go two ways around, clockwise and counter-clockwise.
        -- Would be nice to be able to tell which is which, but it is easier
        -- to just run it twice,
        curve_1 = (s_n_1:go (snd s_n_1) (Set.singleton coords))
        curve_2 = (s_n_2:go (snd s_n_2) (Set.singleton coords))

        go c !seen = case ns of
                      [] -> [head $ filter ((== coords) . snd) $ n_in_map c]
                      (n@(d,c'):_) -> n:go c' (Set.insert c seen)
          where ns = filter (not . flip Set.member seen . snd) $ n_in_map c

        -- calculates the area within the curve as per Green's theorem.
        -- '-' contributes two steps to the East or West, while 'F7LJ' only
        -- contributes one. '|' contributes 0.
        -- We calculate it in multiple of 4, imagining each point as the middle
        -- of the square .|.
        --               - -
        --               .|.
        -- We are going clockwise around the curve, as in the theorem.
        -- If this returns a negative number, the curve was in the wrong
        -- direction.
        greenArea :: [((Dir,[Dir]), (Int,Int))] -> Int
        greenArea = (`div` 4) . greenArea' 0
         where greenArea' !a (c@((East,  p), (y,_)):xs) = greenArea' a' xs
                  where a' = case pipeType p of
                                '-' -> a + (4*y) -- xx
                                                 -- ..
                                'J' -> a + (2*y) -- x.
                                                 -- ..
                                '7' -> a + (2*y) -- xx
                                                 -- .x
               greenArea' !a (c@((West,  p), (y,_)):xs) = greenArea' a' xs
                  where a' = case pipeType p of
                                '-' -> a - (4*y) -- ..
                                                 -- xx
                                'F' -> a - (2*y) -- ..
                                                 -- .x
                                'L' -> a - (2*y) -- x.
                                                 -- xx
               greenArea' !a (c@((North, p), (y,_)):xs) = greenArea' a' xs
                  where a' = case pipeType p of
                                '|' -> a         -- .x
                                                 -- .x
                                'F' -> a + (2*y) -- xx
                                                 -- x.
                                '7' -> a - (2*y) -- ..
                                                 -- x.
               greenArea' !a (c@((South, p), (y,_)):xs) = greenArea' a' xs
                  where a' = case pipeType p of
                                '|' -> a         -- x.
                                                 -- x.
                                'L' -> a + (2*y) -- .x
                                                 -- ..
                                'J' -> a - (2*y) -- xx
                                                 -- xx
               greenArea' !a [] = a

        -- Calculates the area that the curve itself occupies.
        curveArea :: [((Dir,[Dir] ), (Int,Int))] -> Int
        curveArea = (`div` 4) . greenArea' 0
         where greenArea' !a (c@((East,  p), (y,_)):xs) = greenArea' a' xs
                  where a' = case pipeType p of
                                '-' -> a + 2 -- xx
                                             -- ..
                                'J' -> a + 1 -- x.
                                             -- ..
                                '7' -> a + 3 -- xx
                                             -- .x
               greenArea' !a (c@((West,  p), (y,_)):xs) = greenArea' a' xs
                  where a' = case pipeType p of
                                '-' -> a + 2 -- ..
                                             -- xx
                                'F' -> a + 1 -- ..
                                             -- .x
                                'L' -> a + 3 -- x.
                                             -- xx
               greenArea' !a (c@((North, p), (y,_)):xs) = greenArea' a' xs
                  where a' = case pipeType p of
                                '|' -> a + 2 -- .x
                                             -- .x
                                'F' -> a + 3 -- xx
                                             -- x.
                                '7' -> a + 1 -- ..
                                             -- x.
               greenArea' !a (c@((South, p), (y,_)):xs) = greenArea' a' xs
                  where a' = case pipeType p of
                                '|' -> a + 2 -- x.
                                             -- x.
                                'L' -> a + 1 -- .x
                                             -- ..
                                'J' -> a + 3 -- .x
                                             -- xx
               greenArea' !a [] = a




main :: IO ()
main = do
          -- readFile "example-1" >>= print . part1 . lines
          -- readFile "example-2" >>= print . part1 . lines
          -- readFile "example-3" >>= print . part1 . lines
          readFile "input" >>= print . part1 . lines
          -- putStrLn "--"
        --   readFile "input" >>= print . part1 . lines
          -- readFile "example-1" >>= mapM_ print . lines
          -- readFile "example-1" >>= print .  part2 . lines
          -- readFile "example-4" >>= mapM_ print . lines
          -- readFile "example-4" >>= print .  part2 . lines
          -- readFile "example-6" >>= mapM_ print . lines
          -- readFile "example-6" >>= print .  part2 . lines
          -- readFile "example-5" >>= mapM_ print . lines
          -- readFile "example-5" >>= print .  part2 . lines
          readFile "input" >>= print . part2 . lines
