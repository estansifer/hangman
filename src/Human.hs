
module Human (
        human,
        human2,
        human3,
        game_over_message
    ) where

import Data.List (intersperse)
import System.Random (randomRIO)

import Words
import Moderator

human :: PublicState -> IO L
human ps =
    let
        dw  = display_word ps
        g   = guesses ps
        nwg = num_wrong_guesses ps
    in do
        putStrLn $ prompt dw g nwg
        cs <- getLine
        if null cs then human ps else
            let c = head cs in
            if c `elem` alphabet then return c else human ps

prompt :: String -> [L] -> Int -> String
prompt dw g nwg = "    [" ++ intersperse ' ' dw ++ "] {" ++ g ++ "} " ++ show nwg

game_over_message :: PublicState -> String
game_over_message ps = prompt (display_word ps) (guesses ps) (num_wrong_guesses ps)

human2 :: PublicState -> IO L
human2 ps =
    if length (guesses ps) < 4
        then randomRIO alphabet_range
        else human ps

human3 :: PublicState -> IO L
human3 ps = 
    if length (guesses ps) < 6
        then fmap ("aeiouy" !!) $ randomRIO (0, 5)
        else human ps
