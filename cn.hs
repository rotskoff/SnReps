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


instance Group CyclicGroup where
    id = undefined 
    (&) = undefined
    inv = undefined

c :: Int -> CyclicGroup
c n = C {order = n, elems = [1..n-1]}

primitives :: CyclicGroup -> Int
primitives = undefined


