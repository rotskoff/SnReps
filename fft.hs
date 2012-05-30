-- Module	: FFT (Generalized Fast Fourier Transforms)
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

-- Based on Clausen's Fourier Transform on Symmetric Groups

module FFT where

import Sn
import Partitions
import Tableau
import Matrix
import Functions
import qualified Data.Map as Map
import Data.Complex


-- A partially applicable Fourier transform!
-- We use a factorization of into a product of contiguous cycles to execute the recursion
-- Need to pass along the original function
-- Major testing/debugging remains a TODO.
fft :: SnMap -> Partition -> [[Double]]
fft (F f) l
    | (length $ Map.toList f) == 1 = mScale (snd $ head $ Map.toList f) (identityMatrix (dim l))
    | otherwise = foldr1 mSum [ mTimes (yor (o i) l) (fft (mapAdapt (F f) (o i)) l) | i <- [1..n]] where
    o i = fromCycles [[i..n]] n 
    n = factInv $ fDim (F f)


randomFFT :: Partition -> (IO [[Double]])
randomFFT (Part l) = (randomF n) >>= (\s -> return (fft s (Part l))) where
    n = sum l





-- Temp. / Inelegant
factInv :: Int -> Int
factInv x = case x of
              1 -> 1
              2 -> 2
              6 -> 3
              24 -> 4
              120 -> 5
              720 -> 6