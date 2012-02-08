
-- | wordhash.hs
-- | Written by Erin Dahlgren
-- | Feb 8 2012 

-- | inspired by djb2



import Data.Bits
import Data.Char


data Word = Word String Int deriving (Show)
text :: Word -> String
text (Word s _) = s

count :: Word -> Int
count (Word _ c) = c

-- | --

djb2 :: [Char] -> Int
djb2 s = hash 5381 s where
  hash seed [] = seed
  hash seed (c:cs) = hash $ ((shift seed 5) + seed + ord c) cs

jenkins :: [Char] -> Int
jenkins (s:ss) = hash2 $ hash1 s (s:ss) where 
  hash1 seed [] = seed
  hash1 seed (c:cs) = let l = ord c + shift seed 10 in 
          hash1 (xor l (shift l -6)) cs   
  hash2 seed = let (l,r) = (seed + shift seed 3, xor l (shift l -11)) in
    r + shift r 15

-- | --
  
-- | TODO search, add, flatten, and sort