module Main where

import Data.Char (isSpace, isDigit)

example :: [(Int, Int)]
example = [(7,9), (15,40), (30,200)]

input :: [(Int,Int)]
input = [(56,334), (71,1135), (79, 1350), (99, 2430)]


dist :: Num a => a -> a -> a
--dist total_time hold_down_time = (total_time - hold_down_time)*hold_down_time
dist y x = (y -x)*x
-- dist y x == d <=> (y-x)*x == d <==> -1*x^2 +yx -d == 0 (b/sqrt(2))
-- b^2 - 4ac > 0 for two solutions
-- b^2 - 4ac == 0 one solution
-- b^2 - 4ac < 0 no solution
-- x = (-b +- sqrt(b^2 - 4ac))/(2*a)

dist' :: Num a => a -> a -> a
dist' y x = y - (2*x)

solve_xf :: Int -> Int -> (Float,Float)
solve_xf yi di = ((y - sqrt (y*y - 4*d))/2,
                 (y + sqrt (y*y - 4*d))/2)
   where y = fromIntegral yi
         d = fromIntegral di

solve_x :: Int -> Int -> (Int,Int)
solve_x yi di = (ceiling $ (y - sqrt (y*y - 4*d))/2,
                 ceiling $ (y + sqrt (y*y - 4*d))/2)
   where y = fromIntegral yi
         d = fromIntegral di

part1 :: (Int,Int) -> Int
part1 (tt, d) =  if dt == d then (f-t) -1 else (f-t)
  where (t,f) = solve_x tt d
        dt = dist tt t

example2 :: (Int, Int)
example2 = (71530,940200)

input2 :: (Int,Int)
input2 = (56717999, 334113513502430)

main :: IO ()
main = do print $ product $ map part1 example
          print $ product $ map part1 input
          print $ part1 example2
          print $ part1 input2

