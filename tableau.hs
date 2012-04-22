-- Module	: YoungCalculus (Representation Theoretic Operations on Young Diagrams and Tableau)
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

-- Displaying the standard tableau, various parts of the young diagram calculus

module YoungCalculus where

import Partitions
import Sn
import Matrix

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe

newtype YoungTableau = YT [[Int]]

instance Show YoungTableau where
    show (YT []) = ""
    show (YT (x:xs)) = show x ++ "\n" ++ show (YT xs)

standardTableau :: Partition -> YoungTableau
standardTableau (Part a) = YT $ tile (Part a) [1..(sum a)]

tile :: Partition -> [Int] -> [[Int]]
tile (Part []) _ = [] 
tile (Part (x:xs)) a = [take x a] ++ (tile (Part xs) (drop x a)) 

-- TODO: this merits an explanation
actBy :: Permutation -> YoungTableau -> YoungTableau
actBy (Perm a) (YT t) = YT $ map (map (a Map.!)) t  

rowStandard :: YoungTableau -> Bool
rowStandard (YT a) = (map (Set.toList . Set.fromList) a) == a 

columnStandard :: YoungTableau -> Bool
columnStandard (YT a) = (Set.toList $ Set.fromList a) == a

isStandard :: YoungTableau -> Bool
isStandard a = rowStandard a && columnStandard a

content :: YoungTableau -> Int  -> Int
content (YT t) i = ci - ri where
    ri = fromJust $ findIndex (elem i) t
    ci = fromJust $ elemIndex i (t !! ri)

dist :: Int -> Int -> YoungTableau -> Int
dist i j t = (content t j) - (content t i)  

--TODO
-- Young's Orthogonal Representation
yor :: Int -> Partition -> [[Int]] --Irrep
yor i p = undefined


-- TODO
--Columns of YOR
yorColumn :: Int -> Int -> YoungTableau -> (Vector Double)
yorColumn i ci (YT t)
    | isStandard (adjImage i (YT t)) = undefined --makeSparseVec [(ci,d_tt),(___,d_it)] d
    | otherwise = makeSparseVec [((fromIntegral ci),d_tt)] drho where
    d_tt = 1/(fromIntegral d)
    d_it = 1/(1-sqrt(fromIntegral d))
    d = dist i (i+1) (adjImage i (YT t))
    drho = dim $ (Part $ map length t)

-- Action by a transposition
adjImage :: Int -> YoungTableau -> YoungTableau
adjImage i (YT t) = actBy perm (YT t) where
    perm = fromCycles [[i,(i+1)`mod`(order)]] order
    order = sum $ map length t

-- Given a partition, return the list of standard tableau of the same shape
-- TODO: Hard code certain shapes, so the search terminates 
-- Length of list is equal to the dimension of the rep! Terminate if hookLength is reached!
-- Write as a list comprehension
standard :: Partition -> [YoungTableau]
standard (Part a) = filter isStandard $ map (\i -> actBy (s n !! (i-1)) (standardTableau (Part a))) ([1..order]) where
    n = sum a
    order = product [1..n]