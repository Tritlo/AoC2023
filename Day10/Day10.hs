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
parsePipe '-' = [East, West]
parsePipe 'L' = [North, East]
parsePipe 'J' = [North, West]
parsePipe '7' = [South, West]
parsePipe 'F' = [South, East]

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
neighs (x,y) = map (neigh (x,y)) [East, West, North, South]

--part1 :: [String] -> --(Map (Int,Int) MapEntry, (Int,Int))
part1 strs = (1 + go coords Set.empty) `div` 2
  where mp = (Map.fromList . concat . zipWith (\i l -> map (\(j,e) -> ((i,j), e)) l ) [0..] . map (zip [0..] . map parseMap)) strs
        ((coords,Start):[]) = Map.toList $ Map.filter (== Start) mp
        start_ns = neighs coords
        n_in_map c = case mp Map.!? c of
                        Just Start -> mapMaybe f [North, South, East, West]
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


part2 strs = ( max_x, max_y, Set.size edges)
  where mp = (Map.fromList . concat . zipWith (\i l -> map (\(j,e) -> ((i,j), e)) l ) [0..] . map (zip [0..] . map parseMap)) strs
        ((coords,Start):[]) = Map.toList $ Map.filter (== Start) mp
        start_ns = neighs coords

        n_in_map :: (Int,Int) -> [((Dir, [Dir]),(Int,Int))]
        n_in_map c = case mp Map.!? c of
                        Just Start -> mapMaybe f [North, South, East, West]
                        Just (Pipe xs) -> mapMaybe f xs
                        _ -> []

           where f d = let nc = neigh c d
                       in case mp Map.!? nc of
                         Just (Pipe xs) | corresp d `elem` xs -> Just ((d,xs), nc)
                         Just Start -> Just ((d,[]), nc)
                         _ -> Nothing
        curve = go coords Set.empty
        curve_set = Set.fromList $ map snd curve
        go c !seen = case ns of
                      [] -> [head $ filter ((== coords) . snd) $ n_in_map c]
                      (n@(d,c'):_) -> n:go c' (Set.insert c seen)

          where ns = filter (not . flip Set.member seen . snd) $ n_in_map c

        -- calculates the green area, but discretization is somehow wrong.
        greenArea :: [((Dir,[Dir] ), (Int,Int))] -> Int
        greenArea = greenArea' 0
         where
               greenArea' !a (c@((East,_), (y,_)):xs) = greenArea' (a+y) xs
               greenArea' !a (c@((West,_), (y,_)):xs) = greenArea' (a-y) xs
               greenArea' !a (c:xs) = greenArea' a xs
               greenArea' !a [] = a




main :: IO ()
main = do
          readFile "example-1" >>= print . part1 . lines
          putStrLn "---"
          readFile "example-2" >>= print . part1 . lines
          putStrLn "---"
          readFile "example-3" >>= print . part1 . lines
          readFile "input" >>= print . part1 . lines
        --   readFile "input" >>= print . part1 . lines
          readFile "example-1" >>= mapM_ print . lines
          readFile "example-1" >>= print .  part2 . lines
          readFile "example-4" >>= mapM_ print . lines
          readFile "example-4" >>= print .  part2 . lines
          readFile "example-5" >>= mapM_ print . lines
          readFile "example-5" >>= print .  part2 . lines
          readFile "example-6" >>= mapM_ print . lines
          readFile "example-6" >>= print .  part2 . lines