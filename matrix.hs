-- Module	: Matrix
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

-- A minimal implementation of matrix algebra

module Matrix where

data Vector a = V [a] a  
newtype Matrix a = M [Vector a]

