{-# LANGUAGE GHC2021 #-}
module Main where

import qualified Data.Array as Arr
import Data.Array (Array)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (Left, Right)

data Angle = Up | Down | Left | Right
  deriving (Show, Eq, Ord)


opposite Up = Down
opposite Down = Up
opposite Left = Right
opposite Right = Left

data GridEntry = GE [Angle]
  deriving (Show)

parse :: [String] -> Array (Int,Int) GridEntry
parse strs = Arr.array ((0,0), fst (last res)) res
  where parseEntry '.' = GE []
        parseEntry '/' = GE [Up]
        parseEntry '\\' = GE [Down]
        parseEntry '|' = GE [Up, Down]
        parseEntry '-' = GE [Left, Right]
        f = concat . zipWith (\i ls -> map (\(j,e) -> ((i,j),e)) ls) [0..] . map (zip [0..] . map parseEntry)
        res = f strs


move :: (Int,Int) -> Angle -> (Int,Int)
move (y,x) Left = (y, x-1)
move (y,x) Right = (y, x+1)
move (y,x) Up = (y-1, x)
move (y,x) Down = (y+1, x)


fun :: Array (Int,Int) GridEntry -> ((Int,Int), Angle) ->  Int
fun arr e = go Map.empty [e]
   where go :: Map (Int,Int) (Set Angle) -> [((Int,Int), Angle)]  -> Int
         go seen [] = Map.size seen
         go s (((x,y),_):rs) | ((lbx,lby),(ubx,uby)) <- Arr.bounds arr,
                               not (x>= lbx && y >= lby && x <= ubx && y <= uby) = go s rs
         go seen ((bc, bd):other_beams)
             | Just dirs <- seen Map.!? bc,
               bd `Set.member` dirs = go seen other_beams
             | otherwise = let seen' = Map.insertWith Set.union bc (Set.singleton bd) seen
                               new_beam = case arr Arr.! bc of
                                             GE [] -> [(move bc bd, bd)]
                                             GE [Up] -> case bd of -- '/'
                                                          Right -> [(move bc Up,Up)]
                                                          Left ->  [(move bc Down,Down)]
                                                          Up ->    [(move bc Right, Right)]
                                                          Down ->  [(move bc Left, Left)]
                                             GE [Down] -> case bd of -- '\'
                                                          Right -> [(move bc Down,Down)]
                                                          Left ->  [(move bc Up,Up)]
                                                          Up ->    [(move bc Left, Left)]
                                                          Down ->  [(move bc Right, Right)]
                                             GE [Up,Down] -> case bd of -- '|'
                                                          d | d == Left || d == Right
                                                             -> [(move bc Up,Up),
                                                                 (move bc Down, Down)]
                                                          _ -> [(move bc bd, bd)]
                                             GE [Left,Right] -> case bd of -- '-'
                                                          d | d == Up || d == Down
                                                             -> [(move bc Left,Left),
                                                                 (move bc Right, Right)]
                                                          _ -> [(move bc bd, bd)]
                           in go seen' (new_beam <> other_beams)





part1 arr = fun arr ((0,0), Right)

part2 arr = maximum $ map (fun arr) edges
    where ((lbx,lby), (ubx, uby)) = Arr.bounds arr
          edges =  map (\y -> ((y,lbx), Right)) [lby..uby]
                <> map (\y -> ((y,lby), Left))  [lby..uby]
                <> map (\x -> ((lby,x), Down))  [lbx..ubx]
                <> map (\x -> ((uby,x), Up))    [lbx..ubx]


main :: IO ()
main = do readFile "example" >>= print . part1 .  parse . lines
          readFile "input" >>=  print . part1 .  parse . lines
          readFile "example" >>= print . part2 .  parse . lines
          readFile "input" >>= print . part2 .  parse . lines
