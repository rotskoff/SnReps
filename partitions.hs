-- Partitions.hs
-- (c) Grant Rotskoff, 2012
-- A module for the generating partitions of n
-- By a partition of n, we mean a weakly decreasing sequence of integers which sums to n

module Partitions where

import Data.List
import qualified Data.Set as Set

newtype Partition = Part [Int] deriving (Show, Eq, Ord)

table (Part (x:[])) = show $ replicate x '*'
table (Part (x:xs)) = (show $ replicate x '*') ++ "\n" ++ (table (Part xs)) 

partitions :: Int -> [Partition]
partitions 0 = [Part []]
partitions n = [Part (i:xs)| i <- [1..n], (Part xs) <- partitions (n-i),
                        null xs || i >= (head xs)] 

-- Restriction of a representation to S_{n-1}
res :: Partition -> [Partition]
res (Part p) = map Part $ filter (not . null) $ map (\i -> listMod i p) [1..length(p)] where

-- Just a helper function for restriction
listMod :: Int -> [Int] -> [Int]
listMod i p
   | (reverse $ sort res) == res = filter (/=0) res
   | otherwise = [] where 
   ls = splitAt i p
   res = (init $ fst ls)++[(last $ fst ls)-1]++(snd ls) 


hooks :: [Int] -> [Int]
hooks [] = []
hooks a = (hookList a) ++ (hooks dropColumn) where
    dropColumn = (dropWhile (== 0)) $ (map (\s->s-1) a)    
    hookList xs = [i+(xs!!i)|i<-[0..((length xs)-1)]]

-- Calculate the dimension of a representation, via its Young Diagram
-- We convert to arbitrary precision numbers because growth rates are fast. 
dim :: Partition -> Integer
dim (Part a) = (product $ map toInteger [1..(sum a)]) `div` (product $ map toInteger (hooks $ reverse a))
