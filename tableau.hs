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

-- Young's Orthogonal Representation
yor :: Int -> Partition -> [[Int]] --Irrep
yor i p = undefined
    
adjImage :: Int -> Partition -> YoungTableau
adjImage i (Part p) = actBy perm (standardTableau (Part p)) where
    perm = fromCycles ([[i,i+1]]) (sum p) -- TODO MODULUS

