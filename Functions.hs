-- An implementation of functions from a group into the complex numbers
module Functions where

import Sn
import Data.Complex
import qualified Data.Map as Map

newtype SnMap = F (Map.Map Permutation (Complex Double))
    deriving(Show)

identity :: Int -> SnMap
identity n = F $ Map.fromList $ [(p,1)|p<-(s n)]

eval :: SnMap -> Permutation -> (Complex Double)
eval (F f) p = f Map.! p  

k = Map.keys
v = Map.elems

scale :: (Complex Double) -> SnMap -> SnMap
scale a (F f) = F $ Map.fromList $ zip (k f) (map (* a) (v f)) 

add :: SnMap -> SnMap -> SnMap
add (F f) (F g)
    | (length $ Map.toList f) /= (length $ Map.toList g) = error "You may only add functions on the same group!"
    | otherwise = F $ Map.fromList $ zip (k f) vs where
    vs = zipWith (+) (v f) (v g)

