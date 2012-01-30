
-- |
-- Module         : PowerSet
-- License        : BSD
-- Author         : Erin Dahlgren
-- Date           : January 28 2012
-- 
-- Maintainer     : edahlgren@uchicago.edu
-- Stability      : stable-experimental
-- 
-- This module uses arrays and efficient bitwise comparison 
-- to generate the powerset of a settlement pool, in order 
-- to search the bank users in the settlement pool whose 
-- bank rebates generate the highest payback for the settler.  
-- Search constrained by total settlement  <= 645.00 dollars
-- Association lists used for database-like behavior.
-- Recursion is tail-recursive optimized by the haskell compiler.
-- 
-- Comments are via type signatures
-- 

module PowerSet
( user_to_bal, 
  user_to_bank,
  bank_to_rebate,
  merge,
  treat,
  lookup',
  bybit,
  bybit2,
  pset,
  bc,
  maxim,
  main,
) where


import Data.Set
import Data.Bits
import Array
import Text.Printf
import System.Random
import Prelude hiding (map)
  

-- | Sample Data
user_to_bal = [(1,15300),(2,5300),(3,19100),(4,6605),(5,23999),
               (6,13755),(7,14578),(8,24943),(9,4301)]
user_to_bank = [(1,"BofA"),(2,"WFC"),(3,"Chase"),(4,"M&T"),(5,"BB&T"),
                (6,"First National"),(7,"PNC"),(8,"HSBC"),(9,"Bank of the West")]
bank_to_rebate = [("BofA",232),("WFC",73),("Chase",201),("M&T",50),("BB&T",141),
                  ("First National",79),("PNC",48),("HSBC",38),("Bank of the West",133)]



-- | Database-like functionality
merge :: (Eq a) => [t] -> [(a,t2)] -> (t -> t1) -> (t -> a) -> [(t1,t2)]
merge assoc1 assoc2 on1 on2 = [(on1 x, lookup' (on2 x) assoc2) | x <- assoc1]

treat :: [(Integer,Integer)] -> [(Integer,Double)]
treat assoc = [(x, toDollars y) | (x,y) <- assoc] where
  toDollars :: Integer -> Double
  toDollars cents = fromIntegral cents/100

lookup' :: (Eq a) => a -> [(a,t)] -> t
lookup' key ((x,y):xys)
  | key == x = y
  | otherwise = lookup' key xys


-- | Algorithms
bc :: (Num e, Ord e) => [(e,e)] -> e
bc xs = foldr (\(w,x) y -> (+) x y) 0 xs 

bybit2 :: (Num e, Ord e) => Array Int (e, e) -> [[(e, e)]]
bybit2 arr = [ a k | k <- [1..(bit n-1)::Int], (bc $ a k) <= 64500 ] 
    where n = snd $ bounds arr
          a k' = [arr ! i | i <- [1..n], testBit k' (i-1)]

bybit arr = [ subset 0 n k | k <- [1..(bit n-1)::Int] ]
  where n = snd $ bounds arr
        subset sum' 0 _ = []
        subset sum' n' k'
          | ((snd q) + sum') > 64500 = []
          | testBit k' (n'-1) = q : subset ((snd q)+sum') (n'-1) k'
          | otherwise = subset sum' (n'-1) k'
            where q = arr ! n'

pset xs = bybit (listArray (1,length xs) xs)

maxim :: (Num t2, Eq t, Ord t2) => [[(t,t1)]] -> [(t,t2)] -> ([t],t2)
maxim xs assoc = getmax xs ([],0) assoc where
  getmax [] max _ = max
  getmax (x:xs) max assoc
    | snd a > snd max = getmax xs a assoc 
    | otherwise = getmax xs max assoc
      where a = ([a | (a,b) <- x], sum [lookup' c assoc | (c,d) <- x])
                        
                
-- | Testing          
main :: IO ()
main = do
  let ubank = treat $ merge user_to_bank bank_to_rebate fst snd
  let (users,rebates) = maxim (pset user_to_bal) ubank
  printf "We recommend settling the accounts of\n"
  mapM (\x -> printf "User %d\n" x) users
  printf "for a total maximum rebate of %.2f\n" rebates



