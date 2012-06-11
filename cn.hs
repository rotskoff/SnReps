-- Module	: Cn (The Cyclic Group of Order n)
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

-- The cyclic group of order n
module Cn where

import Group

data CyclicGroup = C { order :: Int, elems :: [Int]}
                 deriving (Show)

instance Eq CyclicGroup where
    a == b  = order a == order b 

mult :: CyclicGroup -> Int -> Int -> Int
mult gp a b = (a * b) `mod` (order gp)

invert :: CyclicGroup -> Int -> Int
invert gp a = (lcm a (order gp)) `mod` (order gp) 

c :: Int -> CyclicGroup
c n = C {order = n, elems = [1..n-1]}

--Those elements which generate the group.
primitives :: CyclicGroup -> [Int]
primitives grp = filter (\l -> relPrime (order grp) l) (elems grp)

-- GCD's in the Prelude?
-- TODO take a look at it, make sure it's good.
relPrime :: Int -> Int -> Bool
relPrime x y = (gcd x y) == 1

generate :: Int -> CyclicGroup -> CyclicGroup
generate a grp = C { order = (order grp), elems = ([a] ++ (takeWhile (/= a) $ tail $ iterate (mult grp a) a))}

subgroups :: CyclicGroup -> [CyclicGroup]
subgroups grp = map (\l -> generate l grp) (elems grp) 


