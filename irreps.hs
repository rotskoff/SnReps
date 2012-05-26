-- Module	: Irreps (The irreducible representations of the Symmetric Group)
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental


-- The Irreducible Representations of the Symmetric Group
-- All "irreps" are indexed by Young Diagrams, namely, partitions of n.

module Irreps where

import Sn
import Partitions
import Matrix


showDiagram :: [Int] -> String
showDiagram [] = ""
showDiagram (x:xs) = (show $ replicate x '_') ++ "\n" ++ (showDiagram xs)

irreps :: Int -> [Partition]
irreps = partitions 



 
