-- Partitions.hs
-- (c) Grant Rotskoff, 2012
-- A module for the generating partitions of n
-- By a partition of n, we mean a weakly decreasing sequence of integers which sums to n

module Partitions where

newtype Partition = Part [Int] deriving (Show, Eq, Ord)

table (Part (x:[])) = show $ replicate x '*'
table (Part (x:xs)) = (show $ replicate x '*') ++ "\n" ++ (table (Part xs)) 

partitions :: Int -> [Partition]
partitions 0 = [Part []]
partitions n = [ Part (i:xs) | i <- [1..n], (Part xs) <- partitions (n-i),
                        null xs || i >= (head xs)] 
{-
hookLength :: Partition -> Int
hookLength a = (foldr1 (*) [1..length(a)]) `div` prodHooks where
    prodHooks = undefined
-}