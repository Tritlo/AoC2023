{-# LANGUAGE GHC2021 #-}
module Main where

import Debug.Trace
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c str | (r, _:rs) <- span (not . (==) c) str
                = r:(splitOn c rs)
splitOn c [] = []
splitOn c str = [str]

data Entry = Dot
           | Hash
           | Unknown
  deriving (Eq, Ord)

instance Show Entry where
    show Dot = "."
    show Hash = "#"
    show Unknown = "?"

parseExample :: String -> ([Entry], [Int])
parseExample = parseRow
  where parseRow str = (map toEntry st, read @[Int] $ ("[" ++ nums ++ "]"))
           where [st, nums] = splitOn ' ' str
                 toEntry '.' = Dot
                 toEntry '#' = Hash
                 toEntry '?' = Unknown


arrangement :: [Entry] -> [Int]
arrangement [] = []
arrangement xs = if null hs then xs' else (length hs):xs'
  where (hs, rest) = span (== Hash) xs
        xs' = arrangement $ dropWhile (== Dot) rest

options :: [Entry] -> [[Entry]]
options [] = [[]]
options xs@(Dot:_) | (dots, rest) <- span (== Dot) xs
            = map (dots <>) $ options rest
options xs@(Hash:_) | (hs, rest) <- span (== Hash) xs
            = map (hs <>) $ options rest
options (Unknown:xs) = (map (Dot:) rest) <> (map (Hash:) rest)
    where rest = options xs

options_w_check :: [Int] -> [Entry] -> [[Entry]]
options_w_check [] xs = if all (/= Hash) xs
                        then [replicate (length xs) Dot]
                        else []
options_w_check cs xs | sum cs > length xs = []
options_w_check cs (Dot:xs) = map (Dot:) $ options_w_check cs xs
options_w_check (c:cs) xs@(Hash:_) | (as, bs) <- splitAt c xs
            = if (length as /= c) || any (== Dot) as
              then []
              else case bs of
                     (Hash:_) -> []
                     (Dot:_) -> map (c_hash <>) $ options_w_check cs bs
                     (Unknown:uks) -> map (c_hash <>) $ options_w_check cs (Dot:uks)
                     [] -> [c_hash]
   where c_hash = replicate c Hash
options_w_check (c:cs) xs@(Unknown:rs) |
     (as,bs) <- splitAt c xs
     = --traceShow ((c:cs), as, bs) $
       let make_dot =  map (Dot:) $ options_w_check (c:cs) rs
           make_hash = map (c_hash <>) $ options_w_check cs (Dot:(drop 1 bs))
       in if (length as /= c) || any (== Dot) as
       then make_dot
       else case bs of
               (Hash:_) ->    make_dot
               (Dot:_) ->     make_hash <> make_dot
               (Unknown:_) -> make_hash <> make_dot
               [] -> [c_hash]
  where c_hash = replicate c Hash




calc_opts :: [Int] -> [Entry] -> Int
calc_opts [] xs = if all (/= Hash) xs then 1 else 0
calc_opts cs xs | sum cs > length xs = 0
calc_opts cs (Dot:xs) = calc_opts cs xs
calc_opts (c:cs) xs@(non_dot:rs)
     = if any (== Dot) as
        then make_dot
        else case bs of
             (Hash:_) -> make_dot
             (_:_) -> make_dot + make_hash
             [] -> 1
  where (as,bs) = splitAt c xs
        make_dot =  case non_dot of
                       Hash -> 0
                       _ -> calc_opts (c:cs) rs
        make_hash = calc_opts cs (drop 1 bs)

type Mem = Map ([Int], [Entry]) Int
calc_opts_mem :: Mem -> [Int] -> [Entry] -> (Int, Mem)
calc_opts_mem m [] xs = if all (/= Hash) xs then (1,m) else (0,m)
calc_opts_mem m cs xs | Just r <- m Map.!? (cs, xs) = (r,m)
calc_opts_mem m cs xs | sum cs > length xs = (0,m)
calc_opts_mem m cs (Dot:xs) = calc_opts_mem m cs xs
calc_opts_mem m (c:cs) xs@(non_dot:rs)
     = if any (== Dot) as
        then (make_dot, Map.insert ((c:cs), xs) make_dot m')
        else case bs of
             (Hash:_) -> (make_dot, Map.insert ((c:cs), xs) make_dot m')
             (_:_) -> let v = make_dot + make_hash
                          k = ((c:cs), xs)
                      in (v, Map.insert k v m'')
             [] -> (1, m)
  where (as,bs) = splitAt c xs
        (make_dot, m') = case non_dot of
                           Hash -> (0, m)
                           _ -> calc_opts_mem m (c:cs) rs
        (make_hash, m'') = calc_opts_mem m' cs (drop 1 bs)



part1_simple :: String -> [[Entry]]
part1_simple str = filter ((==) check . arrangement) $  options entries
    where (entries, check) = parseExample str

--part1 :: String ->
part1 str = options_w_check check entries
  where (entries, check) = parseExample str

part1' str = calc_opts check entries
  where (entries, check) = parseExample str


part2 :: String -> [[Entry]]
part2 str = options_w_check check entries
    where (entries', check') = parseExample str
          check = concat $ replicate 5 check'
          entries = concat $ intersperse [Unknown] $ replicate 5 entries'

part2' :: String -> Int
part2' str = calc_opts check entries
    where (entries', check') = parseExample str
          check = concat $ replicate 5 check'
          entries = concat $ intersperse [Unknown] $ replicate 5 entries'

part2'' :: String -> Int
part2'' str = fst $ calc_opts_mem Map.empty check entries
    where (entries', check') = parseExample str
          check = concat $ replicate 5 check'
          entries = concat $ intersperse [Unknown] $ replicate 5 entries'

main :: IO ()
main = do

    readFile "input" >>= print . sum . map part1' . lines
    readFile "example" >>= print . sum . map part2' . lines
    readFile "example" >>= print . sum . map part2'' . lines
    readFile "input" >>= print . sum . map part2'' . lines

    -- readFile "input" >>= mapM_ print . zipWith (\i s -> (i, part2' s)) [1..] . lines
    -- putStrLn "--"
    -- readFile "input" >>= mapM_ print .  map part1 . lines