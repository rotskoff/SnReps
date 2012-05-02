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

-- Untyped for the moment
-- Sum i = 1 .. n
fft f l = sum  
    prefactor = map (\i -> yor l [[1..i]]) [1..n]
    fft adapt f l
     



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