-- Module	: Irreps (The irreducible representations of the Symmetric Group)
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental


{-# LANGUAGE UnicodeSyntax #-} 
-- The Irreducible Representations of the Symmetric Group
-- All "irreps" are indexed by Young Diagrams, namely, partitions of n.

module Irreps where

import Sn
import Partitions
import Matrix

data Irrep = Irrep {name :: Partition, matrix :: Matrix Double}

-- Note that show does not need to compute the matrix, due to Haskell's
-- laziness.
instance Show Irrep where
    show Irrep {name=(Part a),matrix=m} = showDiagram a

showDiagram :: [Int] -> String
showDiagram [] = ""
showDiagram (x:xs) = (show $ replicate x '_') ++ "\n" ++ (showDiagram xs)

irreps :: Int -> [Irrep]
irreps n = map (\s -> Irrep {name=s,matrix=undefined}) (partitions n)

-- We compute the matrix corresponding to a given irrep using Young's Orthogonal representation
youngOrthog :: Partition -> Matrix Double



 
