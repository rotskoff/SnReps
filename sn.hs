-- (c) Grant Rotskoff, 2012
-- This module gives an implementation of the symmetric group on n letters for arbitrary n
-- For efficiency, elements of the group are elements of the type Map Int Int, namely 
-- they are bijections of the set [1..n] onto itself. Cycle notation is also supported. 

module Sn where

import qualified Data.Map as Map
import qualified Data.IntSet as Set
import Group


-- A permutation is represented by a list of integers
newtype Permutation = Perm (Map.Map Int Int)
    deriving (Eq)

instance Show Permutation where
   show (Perm a) = "{ "++(spaces $ Map.toList a)++"}"
spaces [] = ""
spaces (x:xs) = (show x) ++" "++(spaces xs)


-- Generate all permutations of n
perms :: Int -> [[Int]]
perms 1 = [[1]] 
perms n = concatMap (take n . iterate rotate) seeds where
   seeds = map (++[n]) (perms (n-1))
   rotate (x:xs) = xs++[x]

transpositions :: [Int] -> Permutation
transpositions a = Perm $ Map.fromList $ zip a [1..length(a)]

image :: Permutation -> [Int]
image (Perm a) = snd $ unzip $ Map.toList a

compose :: Permutation -> Permutation -> Permutation
compose (Perm a) b = transpositions $ map (a Map.!) (image b)

invert :: Permutation -> Permutation
invert (Perm a) = Perm $ Map.fromList $ zip (image (Perm a)) [1..length(Map.toList a)]

-- We can uniquely represent a permutation as a product of disjoint cycles
-- For efficiency, we will mostly work with the map. Using cycles, however,
-- can facilitate the implementation of some algorithms.

newtype Cycle = Cycle [[Int]]

instance Show Cycle where
   show (Cycle [[]]) = ""   
   show (Cycle (a:as)) = "( " ++ (showSpace a) ++ " )" ++ (show as)

showSpace (x:xs)
   | length (x:xs) == 1 = show x
   | otherwise = show x ++ " " ++ showSpace xs  

nontrivial :: Permutation -> Permutation
nontrivial (Perm a) = Perm $ Map.fromList $ filter (\(a,b) -> a /= b) (Map.toList a)

cycleThru :: Permutation -> Int -> [Int]
cycleThru (Perm a) n = [n]++(takeWhile (/= n) $ iterate (a Map.!) (a Map.! n))

-- TODO: Efficiency, elegance
cycles :: Permutation -> [[Int]]
cycles (Perm a) = [c1] ++ [remains] where
    keys = fst $ unzip $ Map.toList a
    c1 = cycleThru (Perm a) (head keys)
    remains = cycleThru (Perm a) (head $ Set.toList $ 
                                 (Set.fromList keys) Set.\\ (Set.fromList c1)) 

s n = map transpositions $ perms n

instance Group Permutation where
   id n = transpositions [1..n]
   (&) = compose
   inv = invert

conjugateBy :: (Group a) => a -> a -> a
conjugateBy g h = (inv h)&g&h

generate :: (Group a) => a -> [a] -> [a] 
generate a gp = takeWhile (/= a) $ map (\s -> s&a) gp 

-- TODO: Efficiency, no duplicates!
generators :: (Group a) => [a] -> [a] -> [a]
generators elems gp = concatMap (\s -> generate s gp) elems
