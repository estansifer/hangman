
module Computer (
        cheat_computer,
        fair_computer
    ) where

import System.Environment (getArgs)
import System.Random (randomRIO)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Array

import Words
import Moderator

type Dict = [Words.Word]

dict_path :: IO String
dict_path = do
    args <- getArgs
    if null args then return "data/dict" else return $ head args

read_dict :: IO Dict
read_dict = do
    path <- dict_path
    raw_dict <- readFile path
    return $ map compute_word $ lines $ raw_dict


filter_by_length :: Int -> Dict -> Dict
filter_by_length n = filter ((== n) . length . word)

filter_by_mask :: LMask -> Dict -> Dict
filter_by_mask (l, m) = filter ((== m) . (! l) . masks)

partition_by :: (a -> Int) -> [a] -> IntMap [a]
partition_by f = IM.fromListWith (++) . map (\x -> (f x, [x])) . reverse

partition_on_mask :: L -> Dict -> IntMap Dict
partition_on_mask l = partition_by ((! l) . masks)

greatest_by :: Ord k => (a -> k) -> [a] -> IO a
greatest_by f xs = do
    let ys = map (\x -> (f x, x)) xs
    let m = maximum $ map fst ys
    let zs = map snd $ filter ((==m) . fst) ys
    i <- randomRIO (0, length zs - 1)
    return (zs !! i)

best_mask :: (L, Dict) -> IO (Mask, Dict)
best_mask (l, ws) = greatest_by (length . snd) $ IM.toList $ partition_on_mask l ws

type CheatState = Dict

cheat_computer_start :: ComputerStart CheatState
cheat_computer_start = do
    dict <- read_dict
    i <- randomRIO (0, length dict - 1)
    let word1 = dict !! i
    let n = length $ word word1
    l1 <- greatest_by (\l -> length (filter (== l) $ word word1)) alphabet
    let lmask = (l1, masks word1 ! l1)
    return (PublicState {
            word_length = n,
            known_masks = [lmask]
        }, filter_by_mask lmask $ filter_by_length n dict)

cheat_computer_move :: ComputerMove CheatState
cheat_computer_move = best_mask

cheat_computer :: Computer CheatState
cheat_computer = (cheat_computer_start, cheat_computer_move)

type FairState = Words.Word

fair_computer_start :: ComputerStart FairState
fair_computer_start = do
    dict <- read_dict
    i <- randomRIO (0, length dict - 1)
    let word1 = dict !! i
    let n = length $ word word1
    l1 <- greatest_by (\l -> length (filter (== l) $ word word1)) alphabet
    let lmask = (l1, masks word1 ! l1)
    return (PublicState {
            word_length = n,
            known_masks = [lmask]
        }, word1)

fair_computer_move :: ComputerMove FairState
fair_computer_move (l, w) = return (masks w ! l, w)

fair_computer :: Computer FairState
fair_computer = (fair_computer_start, fair_computer_move)
