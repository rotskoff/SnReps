module Matrix where

data Vector a = V [a] a  
newtype Matrix a = M [Vector a]

