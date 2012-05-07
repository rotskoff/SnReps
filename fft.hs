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

-- Untyped for the moment:
-- Sum i = 1 .. n
-- Data Structures!

fft f _ (Part [1]) = eval f (Perm $ Map.fromList [(1,1)]) 
fft f tau l = sum $ fftFactors where
    fftFactors = [ (yor (i..n) l) (fft f' (adapt tau) l) | i <-[f1..n]] 
     



{-
Just some experiments with the C-T FFT...

data Complex = C {real :: Double, imaginary :: Double} deriving (Eq)
instance Show Complex where
    show (C {real=a,imaginary=b}) = (show a) ++ " + " ++ (show b)++"i"


data RealPolynomial = P [Double] deriving (Eq,Show)
data ComplexPolynomial = PC [Complex] deriving (Eq,Show)

fft :: RealPolynomial -> ComplexPolynomial
fft (P (x:xs))
    | null xs = P (map (\s -> C {real=x,imaginary=0}) (x:xs)
    | 

-- Horrible and inefficient; just divide the number 
powersof2 = map (^2) [1..]

pad :: RealPolynomial -> RealPolynomial
pad (P a)
    | length (a) `elem` (powersof2) = (P a)
    | otherwise = undefined


factorContiguous :: [Int] -> [[Int]]
factorContiguous a = [[i,i+1]| i <- a]
-}