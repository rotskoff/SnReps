-- Module	: Matrix
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

-- A minimal implementation of matrix algebra
module Matrix where

import qualified Data.Map as Map

fi = fromIntegral
-- Ints are just a way to index the vectors
-- Note the zero indexing
-- This way we can construct vectors very efficiently for Young's Orthogonal Representation
data Vector a = V (Map.Map Integer a) deriving (Eq)

instance (Show a) => Show (Vector a) where
    show (V m) = showVert $ snd $ unzip $ Map.toList m  

showVert [] = ""
showVert (x:[]) = "|"++(show x)++"|"
showVert (x:xs) = "|"++(show x)++"|"++"\n"++(showVert xs)

-- List of column vectors
newtype Matrix a = M [Vector a]

toVec :: [a] -> Vector a
toVec a = V $ Map.fromList $ zip [0..((fi $ length a) - 1)] a

fromVec :: Vector a -> [a]
fromVec (V a) = snd $ unzip $ Map.toList a 


(?) :: (Vector a) -> Integer -> a
(?) (V m) i = m Map.! i

makeSparseVec :: (Num a) => [(Integer,a)] -> Integer -> Vector a
makeSparseVec kvs n = V $ Map.fromList $ kvs ++ [(i,0)|i<-[0..n-1], not $ i `elem` (fst $ unzip kvs)]
