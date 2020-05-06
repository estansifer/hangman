
module Main (main) where

-- import System.Time (TOD, getClockTime)
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)

import Moderator (play, num_wrong_guesses)
import Human (human, human2, human3, game_over_message)
import Computer (fair_computer, cheat_computer)

type Score = Int

play_round :: IO Score
play_round = do
    ps <- play human cheat_computer
    putStrLn $ game_over_message ps
    return (8 - num_wrong_guesses ps)

now :: IO UTCTime
now = getCurrentTime
    -- (TOD secs picos) <- getClockTime
    -- return (secs * (10 ^ 12) + picos)

to_seconds :: UTCTime -> UTCTime -> Integer
to_seconds start end = ceiling (diffUTCTime end start)
-- to_seconds picos = picos `div` (10 ^ 12)


main :: IO ()
main = do
    begin_time <- now
    scores <- main_loop begin_time 180
    print scores
    putStrLn $ "Total score: " ++ show (sum scores)

main_loop :: UTCTime -> Integer -> IO [Score]
main_loop begin_time timelimit = do
    s <- play_round
    cur_time <- now
    let elaps = to_seconds begin_time cur_time
    putStrLn $ "Time left: " ++ show_time (timelimit - elaps)
    if elaps == timelimit
        then return [s]
        else if elaps > timelimit
            then return [s] -- return []
            else fmap (s:) $ main_loop begin_time timelimit

show_time :: Integer -> String
show_time seconds | seconds < 0 = "time up!"
show_time seconds = show (seconds `div` 60) ++ ":" ++ show_seconds (seconds `mod` 60)
    where
        show_seconds s = if s < 10 then '0' : show s else show s
