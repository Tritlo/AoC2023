{-# LANGUAGE  GHC2021 #-}

module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Char (ord)

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)

import Data.Function (on)

import Data.List (sort)
import Debug.Trace

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]

data Module = FlipFlop { m_id :: Int,
                         connections :: IntSet,
                         is_on :: Bool }
            | Conjunction {m_id :: Int,
                           connections :: IntSet,
                           memory :: IntMap Bool
                           }
            | Broadcaster { m_id:: Int,
                            connections :: IntSet}

instance Eq Module where
    (==) = (==) `on` m_id

instance Ord Module where
    compare = compare `on` m_id


instance Show Module where
    show (Broadcaster _ conns) = "broadcaster -> " <> show conns
    show (Conjunction mid conns m) = "&" <> show mid <> ":" <> show m <> " -> " <> show conns
    show (FlipFlop mid conns s) = "%" <> show mid <> ":" <> show s <> " -> " <> show conns

hash :: String -> Int
hash "broadcaster" = 0
hash "output" = 1
hash "rx" = 2
hash st = hash' 0 0 st
  where hash' cur p [] = cur
        hash' cur p (c:r) = hash' (cur + (ord c)*p10) (p+3) r
            where   p10 = round $ (fromIntegral 10)**(fromIntegral p)

parseModule :: String -> Module
parseModule str@('b':_) | mods <- drop (length "broadcaster -> ") str,
                          modList <- splitOn ',' $ filter (/= ' ') mods
                    = Broadcaster 0 $ IS.fromList $ map hash modList
parseModule   ('%':str) | (nm, rest) <- span (/= ' ') str,
                          modList <- splitOn ',' $ filter (/= ' ') $
                                           drop (length " -> ") rest
                      = FlipFlop (hash nm) (IS.fromList $ map hash modList) False
parseModule   ('&':str) | (nm, rest) <- span (/= ' ') str,
                          modList <- splitOn ',' $ filter (/= ' ') $
                                           drop (length " -> ") rest
                      = Conjunction (hash nm) (IS.fromList $ map hash modList) IM.empty



-- part1 :: [String]-
part1 strs = go init_state (1,0,0) (Seq.singleton (0,0,False))
   where modules = map parseModule strs
         isCon (Conjunction{}) = True
         isCon _ = False
         con_ids = map m_id $ filter isCon modules
         con_input con_id = map m_id $ filter (IS.member con_id . connections) modules
         addInputs (Conjunction m_id conns _) =
                Conjunction m_id conns (IM.fromList $ zip (con_input m_id) (repeat False))
         addInputs m = m
         init_state = (IM.fromList $ map (\m -> (m_id m, m)) $ map addInputs modules)
         go !cur_state cp@(num_ps, chp,clp) Empty
            | num_ps == 1000 = chp*clp
            | otherwise =  let button_press = Seq.singleton (0, 0, False)
                           in go cur_state (num_ps+1,chp,clp) button_press
         go !cur_st (!np,!hp,!lp) ((pid,sid,p_lvl) :<| pulses)
                | pid == rx_h, not p_lvl = np
                | not (pid `IM.member` cur_st) = go cur_st cp' pulses
                | otherwise =
                     let (p, chg) |  Broadcaster {} <- m = (Just p_lvl, id)
                                  |  FlipFlop {} <- m, p_lvl =(Nothing, id)
                                  |  FlipFlop {is_on = s} <- m =
                                            let f (FlipFlop m c s) = FlipFlop m c (not s)
                                            in (Just $ not s, f)
                                      | Conjunction {memory = mem} <- m =
                                           let f (Conjunction m c mem) =
                                                    Conjunction m c (IM.insert sid p_lvl mem)
                                               mem' = IM.insert sid p_lvl mem
                                               pulse = not $ and (IM.elems mem')
                                           in (Just pulse, f)
                         cur_st' = IM.adjust chg mid cur_st
                 in go cur_st' cp' (pulses <> (nsigs p))


            where cp' = if p_lvl then (np, hp+1,lp) else (np,hp, lp+1)
                  m = cur_st IM.! pid
                  mid = m_id m
                  conns = connections m
                  nsigs Nothing = Seq.empty
                  nsigs (Just p) = Seq.fromList $ map (\c -> (c,mid,p))
                                                $ IS.toList conns
                  rx_h = hash "rx"



part2 strs = product $ map (product . map (product . map (sum . map (go_one True)))) flops
   where modules = map parseModule strs
         isCon (Conjunction{}) = True
         isCon _ = False
         con_ids = map m_id $ filter isCon modules
         con_input con_id = map m_id $ filter (IS.member con_id . connections) modules
         addInputs (Conjunction m_id conns _) =
                Conjunction m_id conns (IM.fromList $ zip (con_input m_id) (repeat False))
         addInputs m = m
         init_state = (IM.fromList $ map (\m -> (m_id m, m)) $ map addInputs modules)
         to_acc_rx = map (init_state IM.!) $ con_input (hash "rx")
         to_acc_acc to_acc_rx =
                map (map (init_state IM.!) . IM.keys . memory) to_acc_rx
         flops =  map (map (map (map m_id))) $
                    map (map to_acc_acc) $ map to_acc_acc $ to_acc_acc to_acc_rx
         bf_flops =  map (map (map m_id)) $
                     map to_acc_acc $ to_acc_acc to_acc_rx
         bf_f_flops = map (map m_id) $ to_acc_acc to_acc_rx
         bf_f_f_flops = map m_id $ to_acc_rx
         go_one = go init_state 1 button_press
         button_press = Seq.singleton (0, 0, False)

         go !cur_state num_ps Empty hl flop  = go cur_state (num_ps+1) button_press hl flop
         go !cur_st !np ((pid,sid,p_lvl) :<| pulses) hl flop
                | sid == flop, p_lvl == hl = np
                | not (pid `IM.member` cur_st) = skip
                | otherwise =
                     let (p, chg) |  Broadcaster {} <- m = (Just p_lvl, id)
                                  |  FlipFlop {} <- m, p_lvl =(Nothing, id)
                                  |  FlipFlop {is_on = s} <- m =
                                            let f (FlipFlop m c s) = FlipFlop m c (not s)
                                            in (Just $ not s, f)
                                      | Conjunction {memory = mem} <- m =
                                           let f (Conjunction m c mem) =
                                                    Conjunction m c (IM.insert sid p_lvl mem)
                                               mem' = IM.insert sid p_lvl mem
                                               pulse = not $ and (IM.elems mem')
                                           in (Just pulse, f)
                         cur_st' = IM.adjust chg mid cur_st
                 in go cur_st' np (pulses <> (nsigs p)) hl flop


            where skip = go cur_st np pulses hl flop
                  m = cur_st IM.! pid
                  mid = m_id m
                  conns = connections m
                  nsigs Nothing = Seq.empty
                  nsigs (Just p) = Seq.fromList $ map (\c -> (c,mid,p))
                                                $ IS.toList conns


main :: IO ()
main = do readFile "example" >>= print . part1 . lines
          readFile "example-2" >>= print . part1 . lines
          readFile "input" >>= print . part1 . lines
        --   readFile "input" >>= print . part2 . lines
        --   readFile "example-2" >>= print . part2 . lines
          readFile "input" >>= print . part2 . lines