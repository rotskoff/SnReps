-- Cooley-Tukey Fast Fourier Transform
module FFT where

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