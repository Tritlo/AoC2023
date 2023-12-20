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

    deriving (Eq, Ord)

instance Show Module where
    show (Broadcaster _ conns) = "broadcaster -> " <> show conns
    show (Conjunction mid conns m) = "&" <> show mid <> ":" <> show m <> " -> " <> show conns
    show (FlipFlop mid conns s) = "%" <> show mid <> ":" <> show s <> " -> " <> show conns

hash :: String -> Int
hash "broadcaster" = 0
hash "output" = 1
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
part1and2 stop_at_1000 strs = go Map.empty init_state (1,0,0) (Seq.singleton (0,0,False))
   where modules = map parseModule strs
         isCon (Conjunction{}) = True
         isCon _ = False
         con_ids = map m_id $ filter isCon modules
         con_input con_id = map m_id $ filter (IS.member con_id . connections) modules
         addInputs (Conjunction m_id conns _) =
                Conjunction m_id conns (IM.fromList $ zip (con_input m_id) (repeat False))
         addInputs m = m
         init_state = (IM.fromList $ map (\m -> (m_id m, m)) $ map addInputs modules)
         go !seen_st !cur_state cp@(num_ps, chp,clp) Empty
            | stop_at_1000, num_ps == 1000 = chp*clp
            | Nothing <- seen_before  =
                     let seen_st' = Map.insert cur_state cp seen_st
                         button_press = Seq.singleton (0, 0, False)
                      in go seen_st' cur_state (num_ps+1,chp,clp) button_press
            | Just pp@(npp, php,plp) <- seen_st Map.!? cur_state =
                 let cl = num_ps -npp
                     lpc = clp-plp
                     hpc = chp-php
                     perc = 1_000 `div` cl
                 in (lpc*perc)*(hpc*perc)

            where seen_before = seen_st Map.!? cur_state

         go seen_st !cur_st (!np,!hp,!lp) ((pid,sid,p_lvl) :<| pulses)
                | not stop_at_1000, pid == rx_h, not p_lvl = np
                | not (pid `IM.member` cur_st) = go seen_st cur_st cp' pulses
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
                 in go seen_st cur_st' cp' (pulses <> (nsigs p))


            where cp' = if p_lvl then (np, hp+1,lp) else (np,hp, lp+1)
                  m = cur_st IM.! pid
                  mid = m_id m
                  conns = connections m
                  nsigs Nothing = Seq.empty
                  nsigs (Just p) = Seq.fromList $ map (\c -> (c,mid,p))
                                                $ IS.toList conns
                  rx_h = hash "rx"




main :: IO ()
main = do readFile "example" >>= print . part1and2 True . lines
          readFile "example-2" >>= print . part1and2 True . lines
          readFile "input" >>= print . part1and2 True . lines
          readFile "input" >>= print . part1and2 False . lines