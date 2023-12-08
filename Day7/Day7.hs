{-# LANGUAGE GHC2021 #-}
module Main where

import Data.List
import qualified Data.Set as Set
import Data.Function (on)
import Debug.Trace

cardTypes :: String
cardTypes = "AKQJT98765432"

cardStrength :: Char -> Int
cardStrength '2' = 0
cardStrength '3' = 1
cardStrength '4' = 2
cardStrength '5' = 3
cardStrength '6' = 4
cardStrength '7' = 5
cardStrength '8' = 6
cardStrength '9' = 7
cardStrength 'T' = 8
cardStrength 'J' = 9
cardStrength 'Q' = 10
cardStrength 'K' = 11
cardStrength 'A' = 12

data HandType = HighCard
              | OnePair
              | TwoPair
              | ThreeOfAKind
              | FullHouse
              | FourOfAKind
              | FiveOfAKind
    deriving (Eq, Show, Ord)

handToType :: String -> HandType
handToType str = case reverse $ map length $
                      sortBy (compare `on` length) $ group $ sort str of
                    [5] -> FiveOfAKind
                    [4,1] -> FourOfAKind
                    [3,2] -> FullHouse
                    [3,1,1] -> ThreeOfAKind
                    [2, 2, 1] -> TwoPair
                    [2, 1, 1,1] -> OnePair
                    [1,1,1,1,1] -> HighCard

data Hand = Hand String HandType
   deriving (Eq, Show)

toHand :: String -> Hand
toHand str = Hand str (handToType str)

instance Read Hand where
    readsPrec _ str = [(toHand str, "")]

instance Ord Hand where
    compare (Hand s1 t1) (Hand s2 t2) =
        case compare t1 t2 of
            EQ -> compare (map cardStrength s1) (map cardStrength s2)
            x -> x
example :: [String]
example =
 ["32T3K 765",
  "T55J5 684",
  "KK677 28",
  "KTJJT 220",
  "QQQJA 483"]

parseEntry :: String -> (Hand, Int)
parseEntry str | (hand, _:bid) <- splitAt 5 str
                = (read @Hand hand, read @Int bid)



part1 :: [String] -> Int
part1 =  sum . zipWith (\i (_,b) -> i*b) [1..] .  sortBy (compare `on` fst) . map parseEntry


cardStrength2 :: Char -> Int
cardStrength2 'J' = 0
cardStrength2 '2' = 1
cardStrength2 '3' = 2
cardStrength2 '4' = 3
cardStrength2 '5' = 4
cardStrength2 '6' = 5
cardStrength2 '7' = 6
cardStrength2 '8' = 7
cardStrength2 '9' = 8
cardStrength2 'T' = 9
cardStrength2 'Q' = 10
cardStrength2 'K' = 11
cardStrength2 'A' = 12

handToType2 :: String -> HandType
handToType2 str | num_js == 5 || num_js == 4 = FiveOfAKind
                | num_js == 0 =
                    case non_js_groups of
                        [5] -> FiveOfAKind
                        [4,1] -> FourOfAKind
                        [3,2] -> FullHouse
                        [3,1,1] -> ThreeOfAKind
                        [2, 2, 1] -> TwoPair
                        [2, 1, 1,1] -> OnePair
                        [1,1,1,1,1] -> HighCard
                | num_js == 1 =
                    case non_js_groups of
                        [4] -> FiveOfAKind
                        [3,1] -> FourOfAKind
                        [2,2] -> FullHouse
                        [2, 1, 1] -> ThreeOfAKind
                        [1, 1, 1,1] -> OnePair
                | num_js == 2 =
                    case non_js_groups of
                        [3] -> FiveOfAKind
                        [2,1] -> FourOfAKind
                        [1, 1, 1] -> ThreeOfAKind
                | num_js == 3 =
                    case non_js_groups of
                        [2] -> FiveOfAKind
                        [1,1] -> FourOfAKind

  where (js,non_js) =  partition (== 'J') str
        num_js = length js
        non_js_groups =
            reverse $ map length $ sortBy (compare `on` length) $ group $ sort non_js


compare2 :: Hand -> Hand -> Ordering
compare2 (Hand s1 t1) (Hand s2 t2) = case compare t1 t2 of
                                EQ -> compare (map cardStrength2 s1) (map cardStrength2 s2)
                                x -> x

parseEntry2 :: String -> (Hand, Int)
parseEntry2 str | (hand, _:bid) <- splitAt 5 str
                = (Hand hand (handToType2 hand), read @Int bid)

part2 :: [String] -> Int
part2 =  sum . zipWith (\i (_,b) -> i*b) [1..] .  sortBy (compare2 `on` fst) . map parseEntry2

main :: IO ()
main = do print $ part1 example
          readFile "input" >>= print . part1 . lines
          print $ part2 example
          readFile "input" >>= print . part2 . lines