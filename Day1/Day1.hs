{-# LANGUAGE GHC2021 #-}
module Main where


import Data.Char (isDigit)
import Debug.Trace

example :: [String]
example = [ "1abc2",
          "pqr3stu8vwx",
          "a1b2c3d4e5f",
          "treb7uchet" ]


example2 :: [String]
example2 = [
   "two1nine",
   "eightwothree",
   "abcone2threexyz",
   "xtwone3four",
   "4nineeightseven2",
   "zoneight234",
   "7pqrstsixteen"]

repl :: String -> String
repl str@(x:xs) 
   | ("one", rest)   <- splitAt (length "one")   str = '1':(repl xs)
   | ("two", rest)   <- splitAt (length "two")   str = '2':(repl xs)
   | ("three", rest) <- splitAt (length "three") str = '3':(repl xs)
   | ("four", rest)  <- splitAt (length "four")  str = '4':(repl xs)
   | ("five", rest)  <- splitAt (length "five")  str = '5':(repl xs)
   | ("six", rest)   <- splitAt (length "six")   str = '6':(repl xs)
   | ("seven", rest) <- splitAt (length "seven") str = '7':(repl xs)
   | ("eight", rest) <- splitAt (length "eight") str = '8':(repl xs)
   | ("nine", rest)  <- splitAt (length "nine")  str = '9':(repl xs)
   | otherwise = x:(repl xs)
repl [] = []

main :: IO ()
main = do let f (x:(xs@(_:_))) = [x,last xs]
              f [x] = [x,x]
              f _ = error "f: got empty list!!"
          let part1 = sum . map ( read @Int .  f .  filter isDigit)
          print $ part1 example
          readFile "input" >>= print . part1 . lines
          print $ map repl example2
          print $ part1 $ map repl example2
          readFile "input" >>= print . part1 . map (repl . traceShowId) . lines
