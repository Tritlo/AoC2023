{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.Tree (Tree(..))
import qualified Data.Tree as Tree

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]

data Item = Item {x ::Int,
                  m :: Int,
                  a :: Int,
                  s :: Int}
    deriving (Eq, Show, Ord, Read)

data Result = Accept
            | Reject
            | SendTo String
    deriving (Eq)


instance Show Result where
    show Accept = "A"
    show Reject = "R"
    show (SendTo str) = str

type Rule = (Maybe Condition, Result)


instance {-# OVERLAPPING #-} Show Rule where
    show (Nothing, r) = show r
    show (Just c, r) = show c <> ":" <> show r

data Condition = Cond {l :: Char,
                       lt :: Bool,
                       val :: Int}
    deriving (Eq, Ord)
instance Show Condition where
    show (Cond l lt v) = l:(if lt then '<' else '>'):(show v)

--parse :: String -
parse :: String -> (Map String [Rule], [Item])
parse str | [flows, items] <- splitOn "" (lines str)
            = (Map.fromList $ map parseFlow flows,
               map parseItem items)
  where parseFlow st | (nm, _:rest) <- span (/= '{') st,
                       rules <- splitOn ',' $ filter (/= '}') rest
                      = (nm,map parseRule rules)
        parseResult :: String -> Result
        parseResult "A" = Accept
        parseResult "R" = Reject
        parseResult str = SendTo str

        parseRule :: String -> Rule
        parseRule st | [r] <- splitOn ':' st = (Nothing, parseResult r)
                     | [c,r] <- splitOn ':' st = (Just (parseCond c), parseResult r)
        parseCond :: String -> Condition
        parseCond (c:lt:val) = Cond c (lt == '<') $ read @Int val

        parseItem item = read @Item ("Item " <> item)



-- processItem :: Map String [Rule] -> Item -> Bool
-- processItem =
charToSel :: Char -> (Item -> Int)
charToSel 'x' = x
charToSel 'm' = m
charToSel 'a' = a
charToSel 's' = s

match :: [Rule] -> Item -> Result
match [] _ = Accept
match ((Nothing, r):_) it = r
match ((Just (Cond {..}),r):rs) it =
        if ((charToSel l) it) `comp` val
        then r else match rs it
    where sel = charToSel l
          comp = if lt then (<) else (>)


processItem :: Map String [Rule] -> Item -> Bool
processItem rules it = processItem' "in"
  where processItem' k = case match (rules Map.! k) it of
                         Accept -> True
                         Reject -> False
                         SendTo k' -> processItem' k'

addUp :: Item -> Int
addUp (Item {..}) = x + m + a + s

part1 :: (Map String [Rule], [Item]) -> Int
part1 (rules, items) = sum $ map addUp $ filter (processItem rules) items


type Range = (Int,Int)
decTree :: Map String [Rule] -> Tree (Either Condition Result)
decTree rules = simplify $ decTree' (rules Map.! "in")
  where  decTree' :: [Rule] -> Tree (Either Condition Result)
         decTree' [] = Node (Right Accept) []
         decTree' ((Nothing,r):rs) = lb
            where lb = case r of
                        SendTo s -> decTree' (rules Map.! s)
                        r -> Node (Right r ) []
         decTree' ((Just c,r):rs) = Node (Left c) [lb, decTree' rs]
            where lb = case r of
                        SendTo s -> decTree' (rules Map.! s)
                        r -> Node (Right r ) []
         simplify (Node (Left c) [a,b]) =
            let a' = simplify a
                b' = simplify b
            in if a' == b' then a' else (Node (Left c) [a',b'])
         simplify n = n

accepted :: Map Char Range -> Tree (Either Condition Result) -> Int
accepted ranges (Node (Right Reject) _) = 0
accepted ranges (Node (Right Accept) _) = numAccepted ranges
  where numAccepted :: Map Char Range -> Int
        numAccepted rngs = na
          where na = product $ map (\(x,y)->(y-x)+1) $ Map.elems rngs
accepted ranges (Node (Left (Cond c lt val)) [accept_tree, reject_tree]) =
        (accepted acc_map accept_tree) + (accepted rej_map reject_tree)
    where (acc_aff_range, rej_aff_range) = spl $ ranges Map.! c
          acc_map = flip (Map.insert c) ranges $ acc_aff_range
          rej_map = flip (Map.insert c) ranges $ rej_aff_range
          spl :: Range -> (Range,Range)
          spl (range_min, range_max) =
              let below = (range_min, min range_max (val-1))
                  above = (max range_min (val+1), range_max)
              in if lt
                  then ((range_min, min range_max (val-1)),
                       (max range_min val, range_max))
                  else ((max range_min (val+1), range_max),
                        (range_min, min range_max val))

part2 (rules, _) = accepted ranges dtree
        where toStr (Left c) = show c
              toStr (Right r) = show r
              tree_str =  Tree.drawTree $ fmap toStr $ dtree
              dtree = decTree rules
              ranges :: Map Char Range
              ranges = Map.fromList $ zip ['x','m','a','s'] $ repeat (1,4000)

-- numAccepted :: Map Char Range -> Int
-- numAccepted = product . map (\(x,y) -> y-x) .  Map.elems

{-
     (s < 1351)
     /         \
   px           qqz
 -}

main :: IO ()
main = do readFile "example" >>= print . part1. parse
          readFile "input" >>= print . part1. parse
        --   readFile "example" >>= putStrLn . part2 . parse
        --   readFile "example" >>= print .  part2 . parse
          readFile "example" >>= print .  part2 . parse
          readFile "input" >>= print .  part2 . parse