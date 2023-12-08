{-# LANGUAGE GHC2021, BangPatterns #-}
module Main where

import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace
import qualified Data.IntMap.Strict as IM
import Data.IntMap (IntMap)

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Data.List (group)
import GHC.Real (lcm)

parse :: [String] -> ([Bool], IntMap (Int,Int), (Int,Int))
parse (path:_:nodes) = (map (\c -> case c of
                                    'L' -> True
                                    'R' -> False) path,
                                    IM.fromList $ map (\(n,(ln,rn)) ->
                                        (key_map Map.! n,
                                            (key_map Map.! ln,
                                             key_map Map.! rn))) ps,
                                    (key_map Map.! "AAA",
                                     key_map Map.! "ZZZ")
                                    )
  where node_list = []
        parseNode :: String -> (String, (String,String))
        parseNode str | (n, rest) <- splitAt 3  str,
                        (" = (", rest) <- splitAt 4 rest,
                        (ln, rest) <- splitAt 3 rest,
                        (", ", rest) <- splitAt 2 rest,
                        (nn, rest) <- splitAt 3 rest
                     = (n, (ln, nn))
        ps = map parseNode nodes
        key_map = Map.fromList (zip (map fst ps) [0..])



part1 :: [String] -> Int
part1 str = go (cycle path) 0 aaa
  where (path, nodes, (aaa, zzz)) = parse str
        go (p:ps) !cur_steps cur_node
             | cur_node == zzz = cur_steps
             | (ln, rn) <- nodes IM.! cur_node
                 = go ps (cur_steps + 1) (if p then ln else rn)


parse2 :: [String] -> ([Bool], IntMap (Int,Int), (IntSet, IntSet))
parse2 (path:_:nodes) = (map (\c -> case c of
                                    'L' -> True
                                    'R' -> False) path,
                                    IM.fromList $ map (\(n,(ln,rn)) ->
                                        (key_map Map.! n,
                                            (key_map Map.! ln,
                                             key_map Map.! rn))) ps,
                                    (a_nodes, z_nodes))
  where node_list = []
        parseNode :: String -> (String, (String,String))
        parseNode str | (n, rest) <- splitAt 3  str,
                        (" = (", rest) <- splitAt 4 rest,
                        (ln, rest) <- splitAt 3 rest,
                        (", ", rest) <- splitAt 2 rest,
                        (nn, rest) <- splitAt 3 rest
                     = (n, (ln, nn))
        ps = map parseNode nodes
        node_names = map fst ps
        a_nodes =  IS.fromList $ map (key_map Map.!) $
                        filter (\s -> case s of
                                     (_:_:'A':_) -> True
                                     _ -> False) node_names
        z_nodes =  IS.fromList $
                    map (key_map Map.!) $
                        filter (\s -> case s of
                                     (_:_:'Z':_) -> True
                                     _ -> False) node_names
        key_map = Map.fromList (zip node_names [0..])


part2 :: [String] -> [Int]
part2 str = paths
  where (path, nodes, (aaa, zzz)) = parse2 str
        a_els = IS.elems aaa
        paths = map (go (cycle path) 0) a_els
        go (p:ps) !cur_steps cur_node
             | cur_node `IS.member` zzz = cur_steps
             | (ln, rn) <- nodes IM.! cur_node
                 = go ps (cur_steps + 1) (if p then ln else rn)



main :: IO ()
main = do readFile "example-1" >>= print . part1 . lines
          readFile "example-2" >>= print . part1 . lines
          readFile "input" >>= print . part1 . lines
        --   readFile "example-3" >>= print . parse2 . lines
          readFile "example-3" >>= print . part2 . lines
          readFile "example-3" >>= print . foldr1 lcm . part2 . lines
          readFile "input" >>= print . foldr1 lcm . part2 . lines



