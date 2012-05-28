-- Module	: Sn 
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

-- This module gives an implementation of the symmetric group on n letters for arbitrary n
-- For efficiency, elements of the group are elements of the type Map Int Int, namely 
-- they are bijections of the set [1..n] onto itself. Cycle notation is also supported. 

module Sn where

import Prelude hiding (id)
import qualified Data.Map as Map
import qualified Data.IntSet as Set
import Group


-- Permutations are taken to be maps, which allows for efficient composition.
newtype Permutation = Perm (Map.Map Int Int)
    deriving (Eq,Ord)

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
compose (Perm a) b = transpositions $ map (a `at`) (image b)

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

toCycles :: Permutation -> [[Int]]
toCycles (Perm a)
    | keys == [] = []
    | otherwise = [c1] ++ toCycles (Perm $ Map.fromList remains) where
    keys = fst $ unzip $ Map.toList a
    c1 = cycleThru (Perm a) (head keys)
    remains = [(x,(a `at` x))|x <- keys, not $ x `elem` c1]

-- Specify the Group index
fromCycles :: [[Int]] -> Int -> Permutation
fromCycles a n
    | a == [[n]] = transpositions [1..n] 
    | otherwise = Perm $ Map.fromList $ active ++ ident where
    active
        | (length $ concat a) == 1 = [(1,1)]
        | otherwise = concatMap (\(x:xs) -> [(last xs,x)]++(makeTuples (x:xs))) a
    ident = [(i,i)| i <- [1..n], not $ i `elem` (concat a)]
 
makeTuples (x:[]) = []
makeTuples (x:xs) = [(x,head xs)] ++ makeTuples xs  

s n = map transpositions $ perms n


-- We will represent Sn as a group class. 

instance Group Permutation where
   id n = transpositions [1..n]
   (&) = compose
   inv = invert

size :: Permutation -> Int
size (Perm p) = length $ Map.toList p

conjugateBy :: (Group a) => a -> a -> a
conjugateBy g h = (inv h)&g&h

generate :: (Group a) => a -> [a] -> [a] 
generate a gp = takeWhile (/= a) $ map (\s -> s&a) gp 

-- TODO: Efficiency, no duplicates!
generators :: (Group a) => [a] -> [a] -> [a]
generators elems gp = concatMap (\s -> generate s gp) elems

-- Factorizations for the FFT
-- Young's Orthogonal Representation is built recursively using
-- adjacent transpositions. We can easily factor a contiguous
-- cycle into adjacent transpositions. 

test :: [Int] -> [[Int]]
test [] = [] 
test (x:xs) = reverse $ map (\i -> [x,i]) xs

toTrans :: [[Int]] -> [[Int]]
toTrans cycs = concatMap test cycs    

transToAdj :: [Int] -> [[Int]]
transToAdj [i,j] = undo ++ (reverse $ init undo) where 
    undo = [[k,k+1]| k<-[i..j-1]]  

toAdjacent :: Permutation -> [[Int]]
toAdjacent = (concatMap transToAdj) . toTrans . toCycles 

-- Just for clarity and concision
at = (Map.!)

adapt :: Permutation -> (Permutation,[[Int]])
adapt (Perm p) = (inv(fromCycles o' n)&(Perm p'),o') where
    o' = [[(p `at`  n)..n]]
    n = length $ Map.toList p
    p' = Map.fromList $ init $ Map.toList p

adaptedChain :: Permutation -> [[Int]]
adaptedChain (Perm p) 
    | n == 1 = []
    | otherwise = cyc ++ step  
    where
    cyc = snd $ adapt $ Perm p
    step = adaptedChain $ fst $ adapt $ Perm p
    n = length $ Map.toList p

embed :: Int -> [Permutation]
embed n = map (\(Perm p) -> Perm $ Map.fromList $ (Map.toList p)++[(n+1,n+1)]) (s n)