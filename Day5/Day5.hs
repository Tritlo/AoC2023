{-# LANGUAGE GHC2021 #-}
module Main where

import Data.List (foldl', sort)
import Debug.Trace

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]

data Almanac = Almanac {seeds :: [Integer],
                        maps :: [[(Integer,Integer,Integer)]]
                        } deriving (Show)

inRange :: Integer -> (Integer,Integer,Integer) -> Bool
inRange i (s,_,l) = s <= i
                 && i < s + l

transform :: Integer -> [(Integer,Integer,Integer)]  -> Integer
transform i [] = i
transform i (m@(s,d,r):xs) | inRange i m = (d - s) + i
                           | otherwise = transform i xs

transformAll :: Integer -> [[(Integer,Integer,Integer)]] -> Integer
transformAll = foldl transform



part1 :: String -> Integer
part1 example = minimum $ map (flip transformAll maps) seeds
     where  ([seed_str]:rest) = splitOn ("") $ lines example
            seeds | ("seeds: ",r1) <- splitAt (length "seeds: ") seed_str,
                     nstrs <- splitOn ' ' r1
                    = map (read @Integer) nstrs
            maps = map (map pr . tail) rest
            pr :: String -> (Integer,Integer,Integer)
            pr str | nstrs <- splitOn ' ' str,
                     [a,b,c] <- map (read @Integer) nstrs
                     = (b, a,c)

seedToRanges :: [Integer] -> [(Integer,Integer)]
seedToRanges (x:y:xs) = (x,y):(seedToRanges xs)
seedToRanges [] = []



transformAllRanges :: [(Integer, Integer)] -> [[(Integer, Integer, Integer)]] -> [(Integer, Integer)]
transformAllRanges = foldl' (transformRange .  filter ((> 0) . snd))

transformRange :: [(Integer, Integer)] ->
                  [(Integer,Integer,Integer)] ->
                  [(Integer, Integer)]
transformRange i [] = i
transformRange i (m:ms) = (af <> transformRange uaf ms)
    where  (af, uaf) = transformRange' [] [] i
           transformRange' af uaf [] = (af, uaf)
           transformRange' af uaf (r:rs) =
                 transformRange' (af <> afn) (uaf <> uafn) rs
                where (afn, uafn) = transformOneRange m r


transformOneRange :: (Integer, Integer, Integer) -> (Integer, Integer) -> ([(Integer, Integer)], [(Integer, Integer)])
transformOneRange (s,d,r) (rs, rl) = (map c af, uaf)
  where c :: (Integer,Integer) -> (Integer,Integer)
        c = (\(rs, rl) -> (rs + (d-s), rl))
        r1 = (s, s+r)
        r2 = (rs, rs+rl)
                  -- case 1 [rs rs+rl]   (s   s+r)
        (af, uaf) | rs + rl < s || s+r <= rs = ([], [(rs, rl)])
                  -- case 2 (s  [rs     rs+rl]     s+r)
                  | s <= rs && rs + rl <= s+r = ([(rs, rl)], [])
                  -- case 3 [rs   (s   s+r)   rs+rl]
                  | rs <= s && s+r  <= rs+rl = ([(s, r)], [(rs, s -rs), (s+r, (rs+rl) - (s+r))])
                  -- case 4 [rs  (s rs+rl]    s+r)
                  | rs <= s && rs +rl < s+r && s <= rs + rl =
                        ([(s, rs+rl - s)], [(rs, s - rs),(rs+rl, s+r - (rs+rl))])
                  -- case 5 (s  [rs   s+r)   rs+rl]
                  | s <= rs && rs <= s+r && s+r <= rs+rl =
                        ([(rs, s+r - rs)], [(s+r, rs+rl - (s+r))])
                  | otherwise = error $ "transformRange': uncaught case!\n"  <> show r1 <> "\n" <> show r2
                 -- print $ transformRange' (1, 2, 5) (0, 10)
                 -- [0,1,2,3,4,5,6,7,8,9]
                 -- [(0,1)] <> [(2,5)] <> [(6, 4)]
part2 :: String -> Integer
part2 example =  minimum $ filter (> 0) $
                 map fst $ filter ((>0) . snd) $
                     transformAllRanges (seedToRanges seeds) maps
     where  ([seed_str]:rest) = splitOn ("") $ lines example
            seeds | ("seeds: ",r1) <- splitAt (length "seeds: ") seed_str,
                     nstrs <- splitOn ' ' r1
                    = map (read @Integer) nstrs
            maps = map (map pr . tail) rest
            pr :: String -> (Integer,Integer,Integer)
            pr str | nstrs <- splitOn ' ' str,
                     [a,b,c] <- map (read @Integer) nstrs
                     = (b, a,c)
main :: IO ()
main = do readFile "example" >>= print . part1
          readFile "input" >>= print . part1
          readFile "example" >>= print . part2
          readFile "input" >>= print . part2
        --   print $ transformRange [(79, 14), (55, 13)]
        --                          [(98, 50, 2), (50, 52, 48)]
        --   readFile "input" >>= print . part2