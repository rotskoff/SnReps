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

-- Take an SnMap and return a function from a partition to a sum of matrices

fft f _ (Part [1]) = eval f (Perm $ Map.fromList [(1,1)]) 
fft f tau l = undefined     

