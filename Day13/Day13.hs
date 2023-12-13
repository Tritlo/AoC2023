{-# LANGUAGE GHC2021 #-}
module Main where


import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.List (transpose)
import Debug.Trace

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]

data Entry = Dot
           | Hash
  deriving (Eq, Ord)

instance Show Entry where
    show Dot = "."
    show Hash = "#"



--parseExample :: [String] -> [[Entry]]
parseExample :: [String] -> [[[Entry]]]
parseExample = map parseRow . splitOn ""
   where parseRow :: [String] -> [[Entry]]
         parseRow = map (map toEntry)
           where toEntry '.' = Dot
                 toEntry '#' = Hash
--         parseRow = map (map toEntry)


--oneRefl :: Eq a => Seq a -> IntSet
oneRefl seq = --filter isRefl splits
                IS.fromAscList $ map (Seq.length . fst ) $
                filter isRefl splits
   where splits = map (flip Seq.splitAt seq) [1.. Seq.length seq -1]
         isRefl (as,bs) = as == ref_b || ref_a == bs
            where ref_b = (Seq.reverse $ Seq.take (Seq.length as) bs)
                  ref_a = Seq.take (Seq.length bs) $ Seq.reverse as


reflections :: [[Entry]] -> IntSet
reflections = foldr1 IS.intersection . map (oneRefl . Seq.fromList)

part1 :: [[Entry]] -> Int
part1 e = sum (map (100*) horiz) + sum (verti)
    where horiz = IS.elems $ oneRefl $ Seq.fromList e
          verti = IS.elems $ reflections e

main :: IO ()
main = do readFile "example" >>= print . sum . map part1  . parseExample . lines
          readFile "input" >>= print   . sum . map part1  . parseExample . lines
          --readFile "input" >>= mapM_ print . (map (oneRefl . Seq.fromList)) . head . parseExample . lines
        --   readFile "example" >>= mapM_ print . (map (oneRefl . Seq.fromList)) . head . parseExample . lines