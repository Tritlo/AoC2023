{-# LANGUAGE  GHC2021 #-}
module Main where

import qualified Data.Array as Arr
import Data.Array (Array)
import Control.Monad.ST
import Data.Array.MArray (MArray)
import qualified Data.Array.MArray as MA
import Data.Array.ST.Safe
import Data.STRef
import Data.List (sortBy, sortOn)
import Control.Exception (assert)

import Debug.Trace
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (Left, Right)

data Dir = Up | Down | Left | Right
  deriving (Eq, Ord)

instance Show Dir where
    show Up = "^"
    show Down = "v"
    show Left = "<"
    show Right = ">"


opposite Up = Down
opposite Down = Up
opposite Left = Right
opposite Right = Left

parse :: [String] -> Array (Int,Int) Int
parse strs = Arr.array ((1,1), fst (last res)) res
  where parseEntry c = read @Int [c]
        f = concat . zipWith (\i ls -> map (\(j,e) -> ((i,j),e)) ls) [1..] . map (zip [1..] . map parseEntry)
        res = f strs


move :: (Int,Int) -> Dir -> (Int,Int)
move (y,x) Left = (y, x-1)
move (y,x) Right = (y, x+1)
move (y,x) Up = (y-1, x)
move (y,x) Down = (y+1, x)


from p1@(y1,x1) p2@(y2,x2) | y1 == y2 = if x1 < x2 then Right else Left
                           | x1 == x2 = if y1 < y2 then Down else Up
                           | otherwise = error $ "from: " <> show (p1,p2)




-- dijkstra2D :: Array (Int,Int) Int -> Int
dijkstra2D arr = runST $
   do x :: STArray s (Int,Int) Int <- MA.thaw arr
      -- initialization
      dists <- MA.newArray @(STArray s) bounds (maxBound @Int)
      writeArray dists init 0
      parent <- MA.newArray @(STArray s) bounds Nothing
      visited_ref <- newSTRef Set.empty

      let inBounds p dir = case move p dir of
                      (y,x) | y >= i_y && x >= i_x &&
                              y <= t_y && x <= t_x -> True
                      _-> False
          validNeighs :: (Int,Int) -> ST s [(Int,Int)]
          validNeighs cur = do
             let ib = filter (inBounds cur) [Left,Down, Right, Up]
             res <- do
               mb_cur_parent <- readArray parent cur
               case mb_cur_parent of
                 Nothing -> return ib
                 Just cur_parent -> do
                     mb_cur_parent_parent <- readArray parent cur_parent
                     case mb_cur_parent_parent of
                       Nothing -> return ib
                       Just cur_parent_parent -> do
                          mb_cppp <- readArray parent cur_parent_parent
                          case mb_cppp of
                            Nothing -> return ib
                            Just c_ppp -> do
                             let d_parent_cur = from cur_parent cur
                                 d_parent_parent = from cur_parent_parent cur_parent
                                 d_ppp = from c_ppp cur_parent_parent
                             if d_parent_cur == d_parent_parent &&
                                d_parent_cur == d_ppp
                             then return $ if d_parent_cur == Left || d_parent_cur == Right
                                           then filter (inBounds cur) [Up, Down]
                                           else filter (inBounds cur) [Left, Right]
                             else return $ ib
             return $ map (move cur) res
          go :: [((Int,Int),Int)] -> ST s Int
          go [] = dists `MA.readArray` target
          go ((cur,_):rest) = do visited <- readSTRef visited_ref
                                 cur_dist <- MA.readArray dists cur
                                 writeSTRef visited_ref (Set.insert cur visited)
                                 ns <- filter (not . flip Set.member visited) <$> validNeighs cur
                                 ds_tentative <- mapM (MA.readArray dists) ns
                                 ds_cur <- mapM (fmap (+ cur_dist) . MA.readArray x) ns
                                 let f :: (((Int,Int), Int), Int) -> ST s ((Int,Int), Int)
                                     f ((n,t_dist),cur_dist) | cur_dist <= t_dist
                                           = do writeArray parent n (Just cur)
                                                writeArray dists n cur_dist
                                                return (n, cur_dist)
                                          | otherwise = return (n,t_dist)
                                 ns' <- mapM f $ zip (zip ns ds_tentative) ds_cur
                                --  traceShowM cur
                                 go  (sortOn snd $
                                       filter ((/= cur) . fst ) $
                                       filter (not . flip Set.member visited . fst) (rest <> ns'))
          -- reconstructPath :: (Int,Int) -> ST s [(Int,Int)]
          reconstructPath cur = do c <- readArray parent cur
                                   case c of
                                    Nothing -> return []
                                    (Just p) -> do ps <- reconstructPath p
                                                   return (cur:ps)

      dist_target <- go [(init,0)]
      path <- (reverse <$> reconstructPath target)
      let p_length = map (arr Arr.!) path
      traceShowM path
      traceShowM (p_length)
      traceShowM (sum p_length)
      return path
  where bounds@(init@(i_y,i_x),target@(t_y,t_x)) = Arr.bounds arr


parse2 :: [String] -> [[((Int,Int),Int)]]
parse2 strs = res
  where parseEntry c = read @Int [c]
        f = zipWith (\i ls -> map (\(j,e) -> ((i,j),e)) ls) [1..] . map (zip [1..] . map parseEntry)
        res = f strs


db file = do
          path <- readFile file >>= return . dijkstra2D . parse . lines
          p2 <- readFile file >>= return . parse2 . lines
          let full_path = (1,1):path
              getDirs (x:y:res) = (x,from y x):getDirs (y:res)
              getDirs _ = []
              p_w_dirs = Map.fromList $ getDirs $ reverse full_path
              f (c,v) = case p_w_dirs Map.!? c of
                          Just d -> show d
                          _ -> show v
          mapM_ (\ls -> putStrLn "" >> mapM (putStr . f) ls) p2

main :: IO ()
main = do -- readFile "example" >>= print . parse . lines
          --let heur (a,b) (c,d) = compare (c*c + d*d) (a*a + b*b)
          -- readFile "example" >>= print .  dijkstra2D . parse . lines
          -- readFile "input" >>= print .  dijkstra2D . parse . lines
          -- db "example-m"
          db "example"
          -- db "input"