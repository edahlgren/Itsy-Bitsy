
-- |
-- A simple test framework for randomly generating
-- settlement pools.  
-- 
-- Range of settlements follows problem:
-- 40.00 to 240.00 dollars
-- Range of rebates follows problem:
-- 0.40 to 2.40 dollars
-- 
-- To increase bank user size:
-- rand_assoc _ 4000 24000 
-- rand_assoc _ 40 240
-- 
-- Author : Erin Dahlgren
-- Date   : January 28 2012
-- 


import System.Random
import Text.Printf
import System.Environment
import Control.Monad
import Data.Bits
import Array
import PowerSet hiding (main)


main :: IO ()
main = do
  b <- rand_assoc 20 4000 24000
  r <- rand_assoc 20 40 240
  let (users, reb) = maxim (pset b) (treat r)
  printf "We recommend settling the accounts of\n"
  mapM (\x -> printf "User %d\n" x) users
  printf "for a total maximum rebate of %.2f\n" reb
  

rand_assoc :: Int -> Int -> Int -> IO [(Integer, Integer)]
rand_assoc num from to = do
  let a = [y | y <- [1..(fromIntegral num)]]
  x <- replicateM num $ randomRIO (fromIntegral from, fromIntegral to)
  return $ zip a x






