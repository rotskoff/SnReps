-- Module	: FFT (Generalized Fast Fourier Transforms)
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental


-- Cooley-Tukey Fast Fourier Transform
module FFT where

import Sn
import Partitions
import Tableau
import Matrix
import Functions
import qualified Data.Map as Map
import Data.Complex
import System.Random

-- Take an SnMap and return a function from a partition to a sum of matrices

rand :: IO Double
rand = do
  x <- randomRIO (-1,1)
  return (x :: Double)


order :: Partition -> Int
order (Part l) = sum l

-- Need to return a coefficient matrix and an iterable type
-- Data structure for iteration.

fullFFT :: SnMap -> [(Partition,[[Double]])]
fullFFT = undefined

-- A partially applicable Fourier transform!
-- We use a factorization of into a product of contiguous cycles to execute the recursion.
fft :: SnMap -> Partition -> [[Double]]
fft f (Part [1]) = [[eval f (Perm $ Map.fromList [(1,1)])]] 
fft f l = -- Recursive Step


directSum :: (Num a) => [[a]] -> [[a]] -> [[a]]
directSum m1 m2 = block1 ++ block2 where
    block1 = map (++ zeros1) m1
    block2 = map (zeros2 ++) m2
    zeros1 = replicate (length m2) 0
    zeros2 = replicate (length m1) 0
