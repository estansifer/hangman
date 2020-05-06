
module Main (main) where

import Words

is_valid :: String -> Bool
is_valid w = all (`elem` alphabet) w && (length w >= 8)

main = interact (unlines . filter is_valid . lines)
