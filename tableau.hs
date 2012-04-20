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

newtype YoungTableau = YT [[Int]]

instance Show YoungTableau where
    show (YoungTableau []) = ""
    show (YoungTableau (x:xs)) = show x ++ "\n"

standardTableau :: Partition -> YoungTableau
standardTableau = YT $ tile

tile :: Partition -> [[Int]]
tile (Part []) = [[]]
tile (Part (x:xs)) = take x [head(x)..sum(x:xs)] ++ tile xs

actBy :: Permutation -> YoungTableau -> YoungTableau
actBy (Perm a) (YT (x:xs)) = undefined -- just map the action over the lists

isStandard :: YoungTableau -> Bool
isStandard = undefined

content :: YoungTableau -> Permutation -> Int
content = undefined


