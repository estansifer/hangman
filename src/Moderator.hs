
module Moderator (
        PublicState(..),
        display_word, guesses, wrong_guesses, num_wrong_guesses,
        Human, ComputerStart, ComputerMove, Computer,
        play
    ) where

import Data.Bits ((.|.), (.&.), testBit)
import Words

data PublicState = PublicState {
        word_length :: Int,
        known_masks :: [LMask]
    }

word_mask :: PublicState -> Mask
word_mask ps = 2 ^ (word_length ps) - 1

display_word :: PublicState -> String
display_word ps = reverse $ foldr
    (\(l, m) -> zipWith
        (\b l2 -> if b then l else l2)
        (map (testBit m) [0..]))
    (replicate (word_length ps) blank)
    (known_masks ps)

finished :: PublicState -> Bool
finished ps = (foldr (.|.) 0 $ map snd $ known_masks ps) == (word_mask ps)

guesses :: PublicState -> [L]
guesses = reverse . map fst . known_masks

wrong_guesses :: PublicState -> [L]
wrong_guesses = reverse . map fst . filter ((==0) . snd) . known_masks

num_wrong_guesses :: PublicState -> Int
num_wrong_guesses = length . wrong_guesses

type Human = PublicState -> IO L

type ComputerStart a = IO (PublicState, a)
type ComputerMove a = (L, a) -> IO (Mask, a)

type Computer a = (ComputerStart a, ComputerMove a)

play :: Human -> Computer a -> IO PublicState
play human (computer_start, computer_move) = computer_start >>= play_repeat where
    play_repeat (ps, a) = do
        l <- human ps
        if l `elem` guesses ps then play_repeat (ps, a) else do
            (m, a') <- computer_move (l, a)
            let ps' = ps {
                    known_masks = (l, m) : known_masks ps
                }
            if finished ps'
                then return ps'
                else play_repeat (ps', a')
