{-# LANGUAGE GHC2021 #-}
module Main where

import Data.List (transpose)
import qualified Data.Map.Strict as Map

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]

data Entry = Dot
           | Hash
           | Round
  deriving (Eq, Ord)

instance Show Entry where
    show Dot = "."
    show Hash = "#"
    show Round = "O"

parse :: [String] -> [[Entry]]
parse = map (map toEntry)
  where toEntry '.' = Dot
        toEntry '#' = Hash
        toEntry 'O' = Round

countLoad :: [[Entry]] -> Int
countLoad entries = sum $ zipWith (\i n -> i*n) [1 ..length entries] $
                          map numRocks entries
   where numRocks = length . filter (== Round)

tilt :: [[Entry]] -> [[Entry]]
tilt (x:y:xs) = x':(tilt (y':xs))
  where tilt1 Round Dot = (Dot, Round)
        tilt1 x y = (x,y)
        (x',y') = unzip $ map (uncurry tilt1) $ zip x y
tilt xs = xs

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a = let a' = f a
               in if a' == a then a' else fixpoint f a'

part1 :: [String] -> Int
part1 = countLoad . fixpoint tilt . reverse . parse

part2 strs = countLoad $ reverse $ its !! (ls + (cycs - ls) `mod` (lr - ls))
  where es = parse strs
        cycle = transpose . fixpoint tilt . transpose . -- East
                fixpoint tilt . -- South
                transpose . reverse . fixpoint tilt . reverse . transpose . -- West
                reverse . fixpoint tilt . reverse -- North
        cycs = 1_000_000_000
        its = iterate cycle es
        (ls, lr) = go Map.empty 0 es
        go !seen !n es =  case seen Map.!? es of
                            Just n' -> (n', n)
                            _ -> go (Map.insert es n seen) (n+1) (cycle es)




main :: IO ()
main = do
        --   readFile "example" >>= mapM_ print . reverse . parse . lines
          readFile "example" >>= print . countLoad . fixpoint tilt .  reverse . parse . lines
        --   readFile "example" >>= mapM_ print
        --                         . fixpoint tilt . reverse . parse . lines
        --   putStrLn "--"
            --mapM_ (\l -> putStrLn "--" >> mapM_ print l)
          readFile "input" >>= print . part1 . lines
          readFile "example" >>= print . part2 . lines
          readFile "input" >>= print . part2 . lines
