
module Words (
        L, W, Words.Word(..),
        alphabet_range, alphabet, blank,
        Mask, LMask, Masks,
        compute_word
    ) where

import Data.Array as A
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import System.Random

type L = Char

alphabet_range :: (L, L)
alphabet_range = ('a', 'z')

alphabet :: [L]
alphabet = A.range alphabet_range

blank :: L
blank = '_'

type W = [L]

type Mask = Int

type LMask = (L, Mask)

type Masks = Array L Mask

data Word = Word {
        word :: W,
        masks :: Masks
    }

compute_mask :: W -> L -> Mask
compute_mask w l = to_binary 0 $ map (== l) w where
    to_binary k [] = k
    to_binary k (True :bs) = to_binary (2 * k + 1) bs
    to_binary k (False:bs) = to_binary (2 * k)     bs

arrayize :: Ix k => (k -> v) -> (k, k) -> Array k v
arrayize f bounds = A.array bounds $ map (\i -> (i, f i)) $ A.range bounds

compute_word :: W -> Words.Word
compute_word w = Word {
        word = w,
        masks = arrayize (compute_mask w) alphabet_range
    }
