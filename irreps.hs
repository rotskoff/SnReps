{-# LANGUAGE UnicodeSyntax #-} 
-- The Irreducible Representations of the Symmetric Group
-- All "irreps" are indexed by Young Diagrams, namely, partitions of n.

module Irreps where

import Sn
import Partitions
import Matrix

data Irrep = Irrep {name :: Partition, matrix :: Matrix Int}

-- Note that show does not need to compute the matrix, due to Haskell's
-- laziness.
instance Show Irrep where
    show Irrep {name=(Part a),matrix=m} = showDiagram a

showDiagram :: [Int] -> String
showDiagram [] = ""
showDiagram (x:xs) = (show $ replicate x '*') ++ "\n" ++ (showDiagram xs)

irreps :: Int -> [Irrep]
irreps n = map (\s -> Irrep {name=s,matrix=undefined}) (partitions n)

 