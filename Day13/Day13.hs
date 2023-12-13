{-# LANGUAGE GHC2021 #-}
module Main where


import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.List (transpose, partition)
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

parseExample :: [String] -> [[[Entry]]]
parseExample = map parseRow . splitOn ""
   where parseRow :: [String] -> [[Entry]]
         parseRow = map (map toEntry)
           where toEntry '.' = Dot
                 toEntry '#' = Hash


oneRefl :: Eq a => Seq a -> IntSet
oneRefl seq = --filter isRefl splits
                IS.fromAscList $ map (Seq.length . fst ) $
                filter isRefl splits
   where splits = map (flip Seq.splitAt seq) [1.. Seq.length seq -1]
         isRefl (as,bs) = res_1 || res_2
            where ref_b = (Seq.reverse $ Seq.take (Seq.length as) bs)
                  ref_a = Seq.take (Seq.length bs) $ Seq.reverse as
                  res_1 = as == ref_b
                  res_2 = ref_a == bs



reflections :: [[Entry]] -> IntSet
reflections = foldr1 IS.intersection . map (oneRefl . Seq.fromList)

part1 :: [[Entry]] -> Int
part1 e = sum (map (100*) horiz) + sum (verti)
    where horiz = IS.elems $ oneRefl $ Seq.fromList e
          verti = IS.elems $ reflections e



oneChange :: [[Entry]] -> [[[Entry]]]
oneChange es@(e:_) = map change indices
  where indices = (\a b -> (a,b)) <$> [0.. length e -1] <*> [0.. length es -1]
        change (i,j) = a <> ((x <> (opposite y:ys)):bs)
            where (a,b:bs) = splitAt j es
                  (x,y:ys) = splitAt i b
        opposite Dot = Hash
        opposite Hash = Dot


--part2 :: [[Entry]] -> Int
part2 :: [[Entry]] -> Int
part2 e = r
   where (p1:_) = part1' e
         part1' :: [[Entry]] -> [Int]
         part1' e = verti <> (map (100*) horiz)
             where horiz = IS.elems $ oneRefl $ Seq.fromList e
                   verti = IS.elems $ reflections e
         (r:_) = filter (/= p1) $ (concatMap part1' $ oneChange e)


main :: IO ()
main = do readFile "example" >>= print . sum . map part1  . parseExample . lines
          readFile "input" >>= print   . sum . map part1  . parseExample . lines
          readFile "example" >>= print . sum . map part2  . parseExample . lines
          readFile "input" >>= print . sum . map part2  . parseExample . lines