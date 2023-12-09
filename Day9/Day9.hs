{-# LANGUAGE GHC2021 #-}
module Main where




example :: [String]
example = [
   "0 3 6 9 12 15",
   "1 3 6 10 15 21",
   "10 13 16 21 30 45"]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]

parseExample :: [String] -> [[Int]]
parseExample = map parseExample'
 where parseExample' = map (read @Int) . splitOn ' '


extrapolate :: [Int] -> [Int]
extrapolate (x:y:xs) = (y-x):(extrapolate (y:xs))
extrapolate _ = []

part1 :: [Int] -> Int
part1 seq = last $ go sq
  where (s:ss) = reverse $ (takeWhile (not . (all (== 0))) . iterate extrapolate) seq
        sq = (0:) $ map (head . reverse) $ s:ss
        go (x:y:xs) = (y+x):(go ((y+x):xs))
        go _ = []

part2 :: [Int] -> Int
part2 seq = last $ go sq -- (ns:s:ss)
  where (s:ss) = reverse $ (takeWhile (not . (all (== 0))) . iterate extrapolate) seq
        ns = replicate (length s - 1) 0
        sq =  map head $ ns:s:ss
        go :: [Int] -> [Int]
        go (x:y:xs) = (y-x):(go ((y-x):xs))
        go _ = []


main :: IO ()
main = do ---print $ parseExample example
          print $ sum $ map part1 $ parseExample example
          readFile "input" >>= print . sum . map part1 . parseExample . lines
          print $ sum $ map part2 $ parseExample example
          readFile "input" >>= print . sum . map part2 . parseExample . lines