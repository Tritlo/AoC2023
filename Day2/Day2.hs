{-# LANGUAGE GHC2021 #-}
module Main where

import Data.Char (isDigit,isSpace)

example :: [String]
example = [
   "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ]


data Game = Game { gid :: Int,
                   hands :: [Hand]}
  deriving (Show)

data Hand = Hand { red :: Int,
                   green :: Int,
                   blue :: Int
                   }
  deriving (Show)

addHands (Hand b1 r1 g1) (Hand b2 r2 g2) = Hand (b1+b2) (r1+r2) (g1+g2)

maxHand (Hand r1 g1 b1) (Hand r2 g2 b2) = Hand (max r1 r2) (max g1 g2) (max b1 b2)

parse :: String -> Game
parse str | ("Game ", rest) <- splitAt (length "Game ") str,
            (idstr, rest)   <- span isDigit rest,
            rest <- drop 2 rest,
            handstrs <- splitOn ';' rest

            = (Game {gid = read @Int idstr, hands = map parseHand handstrs})

parseHand :: String -> Hand
parseHand str | els <- map (parseSimpleHand . dropWhile isSpace) (splitOn ',' str)
            = foldl addHands (Hand 0 0 0) els


parseSimpleHand str | (num, xs) <- span isDigit str
        = case xs of
            " green" -> Hand {green = read @Int num, red = 0, blue = 0}
            " blue" -> Hand {blue = read @Int num, red = 0, green = 0}
            " red" -> Hand {red = read @Int num, blue = 0, green = 0}


splitOn :: Char -> String -> [String]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]

possible :: Hand -> Game -> Bool
possible (Hand r g b) (Game {hands = hands})
   = all possibleHand hands
  where possibleHand (Hand rc gc bc) =  and [rc <= r,
                                             gc <= g,
                                             bc <= b]

main :: IO ()
main = do let part1 = sum . map gid . filter (possible (Hand 12 13 14)) . map parse
          print $ part1 example
          readFile "input" >>= print . part1 . lines
          let part2 = sum . map ((\(Hand r g b) -> r*g*b) .foldl maxHand (Hand 0 0 0) . hands) . map parse
          print $ part2 example
          readFile "input" >>= print . part2 . lines


