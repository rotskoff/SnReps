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

-- For a list of ints in descending order

hooks :: [Int] -> [Int]
hooks [] = []
hooks a = (hookList a) ++ (hooks dropColumn) where
    dropColumn = (dropWhile (== 0)) $ (map (\s->s-1) a)    
    hookList xs = [i+(xs!!i)|i<-[0..((length xs)-1)]]

-- Calculate the dimension of a representation, via its Young Diagram
-- We convert to arbitrary precision numbers because growth rates are fast. 
dim :: Partition -> Integer
dim (Part a) = (product $ map toInteger [1..(sum a)]) `div` (product $ map toInteger (hooks $ reverse a))
