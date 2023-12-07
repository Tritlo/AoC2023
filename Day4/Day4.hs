{-# LANGUAGE GHC2021 #-}
module Main where


import Data.Char (isDigit)
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set

example :: [String]
example = [
   "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
 ]

data Card = Card { cid :: Int,
                   nums :: [Int],
                   winning :: [Int]
                   }
    deriving (Show)


splitOn :: Char -> String -> [String]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]


parseCard :: String -> Card
parseCard str | ("Card", rest) <- splitAt 4 str,
                rest <- dropWhile (not . isDigit) rest,
                (cidstr, _:rest) <- span isDigit rest,
                [nstr, wstr] <- splitOn '|' rest,
                nstrs <- filter (not . null) $ splitOn ' ' nstr,
                wstrs <- filter (not . null) $ splitOn ' ' wstr

                 = Card (read @Int cidstr) (map (read @Int) nstrs) (map (read @Int) wstrs)

numWinning :: Card -> Int
numWinning (Card _ ns ws) = Set.size $ (Set.fromList ns) `Set.intersection` (Set.fromList ws)

part1 :: [String] -> Int
part1 example = sum $ map (round . f . fromIntegral .numWinning . parseCard) example
    where f n = if n > 0 then (2 ** (n-1)) else 0

part2 :: [String] -> Int
part2 example = sum $ map fst $ go start
  where xs = map ((\c@(Card cid _ _) -> (cid, numWinning c) ) . parseCard ) example
        start = map (1,) xs
        go :: [(Int,(Int,Int))] -> [(Int, (Int,Int))]
        go [] = []
        go (c@(n, (_, w)):xs) = c:(go (added' <> rest))
            where (added,rest) = splitAt w xs
                  added' = map (\(an,c) -> (n+an,c)) added


main :: IO ()
main = do print $ part1 example
          readFile "input" >>= print . part1 . lines
          print $ part2 example
          readFile "input" >>= print . part2 . lines
