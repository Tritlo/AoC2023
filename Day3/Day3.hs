{-# LANGUAGE GHC2021 #-}
module Main where

import Data.List 
import Data.Char
import Debug.Trace
import Data.Traversable (for)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

example :: [String]
example = [
   "467..114..",
   "...*......",
   "..35..633.",
   "......#...",
   "617*......",
   ".....+.58.",
   "..592.....",
   "......755.",
   "...$.*....",
   ".664.598.."]


symbolLocs :: String -> [Int]
symbolLocs = sl
  where sl :: String -> [Int]
        sl x = findIndices f x
        f x | isDigit x = False
            | isAlphaNum x = False
            | '.' <- x = False
            | otherwise = True

parseLine :: String -> [(Int,Int,Int)]
parseLine str = parseLine' 0 str
 where parseLine' i str | (x,xs) <- span (not . isDigit) str,
                          (y@(_:_), ys) <- span isDigit xs,
                          si <- i + length x,
                          ei <- si + length y
                          = (read @Int y, si, ei-1):parseLine' ei ys
       parseLine' _ _ = []




solve :: [String] -> Int
solve whole@(x:y:rest) =  sum $ concat $ solveFirst [x,y]:solveWindow whole
  where solveFirst [x,y] = solveLine x (concatMap symbolLocs [x,y])
        solveWindow :: [String] -> [[Int]]
        solveWindow (x:y:z:rest) =
            solveLine y  (concatMap symbolLocs [x,y,z]):(solveWindow (y:z:rest))
        solveWindow [x,y] = [solveLine y (concatMap symbolLocs [x,y])]
        solveLine :: String -> [Int] -> [Int]
        solveLine ln  syms = map (\(n,_,_) -> n) $
                            (filter (\n -> any (\f -> f n) $ map touches syms) nums)
            where nums = parseLine ln
                  touches si (_,ns, ne) = (ns <= si && si <= ne)
                                          || si == (ns -1)
                                          || si == ne + 1

solve2 :: [String] -> Int
solve2 whole = sum $ map product $  Map.elems $ Map.filter (\v -> length v == 2) combMap 
  where (x:y:rest) = zip [0..] whole
        gearlocs :: (Int, String) -> [(Int, Int)]
        gearlocs (i,xs) = map (i,) $ (findIndices (== '*') xs)
        solveFirst [x,y] = solveLine x (concatMap gearlocs [x,y])
        solveWindow :: [(Int, String)] -> [[(Int, [(Int,Int)])]]
        solveWindow (x:y:z:rest) =
            solveLine y  (concatMap gearlocs [x,y,z]):(solveWindow (y:z:rest))
        solveWindow [x,y] = [solveLine y (concatMap gearlocs [x,y])]

        combMap = foldr addToMap Map.empty $ concat $ solveFirst [x,y]:solveWindow (x:y:rest)
        solveLine :: (Int, String) -> [(Int,Int)] -> [(Int, [(Int,Int)])]
        solveLine ln syms = filter (not . null . snd) $ map f nums
                            
            where nums :: [(Int, Int, Int)]
                  nums = parseLine $ snd ln
                  touches (_,si) (_,ns, ne) = (ns <= si && si <= ne)
                                            || si == (ns -1)
                                            || si == ne + 1
                  f :: (Int,Int,Int) -> (Int, [(Int,Int)])
                  f n@(v,_,_) = (v, filter (flip touches n) syms)

        addToMap :: (Int, [(Int,Int)]) -> Map (Int,Int) [Int] -> Map (Int,Int) [Int]
        addToMap (v, keys) om = Map.unionWith (<>) om nm
          where nm = Map.fromList (zip keys (repeat [v]))

       

main :: IO ()
main = do mapM print example
          -- print $ map symbolLocs example
          -- print $ map parseLine example
          print $ solve example
          readFile "input" >>= print . solve . lines
          print $ solve2 example
          readFile "input" >>= print . solve2 . lines


         

