-- An implementation of functions from a group into the complex numbers
module Functions where

import Sn
import Group
import Data.Complex
import Data.List (unfoldr)
import System.Random
import qualified Data.Map as Map

newtype SnMap = F (Map.Map Permutation Double)
    deriving(Show)

identity :: Int -> SnMap
identity n = F $ Map.fromList $ [(p,1)|p<-(s n)]

eval :: SnMap -> Permutation -> Double
eval (F f) p = f Map.! p  

k = Map.keys
v = Map.elems

fDim :: SnMap -> Int
fDim (F f) = length (k f)

scale :: Double -> SnMap -> SnMap
scale a (F f) = F $ Map.fromList $ zip (k f) (map (* a) (v f)) 

add :: SnMap -> SnMap -> SnMap
add (F f) (F g)
    | (length $ Map.toList f) /= (length $ Map.toList g) = error "You may only add functions on the same group!"
    | otherwise = F $ Map.fromList $ zip (k f) vs where
    vs = zipWith (+) (v f) (v g)

randomls :: Int -> StdGen -> [Double]
randomls n = take n . unfoldr (Just . randomR (0,1))

randomF :: Int -> IO SnMap
randomF n = do
  seed <- newStdGen
  let ks = s n
  let vs = randomls (length ks) seed
  return $ F $ Map.fromList $ zip ks vs

-- A key step to the recursion of the Fourier Transform is the adaptation of
-- SnMaps to the subgroups of Sn. Essentially, this is a precise way of 
-- restricting the function to subgroups.
-- We build a function on the subgroup in terms of its values on the larger group via
-- a factorization into contiguous cycles.  
mapAdapt :: SnMap -> Permutation -> SnMap
mapAdapt (F f) o = F $ Map.fromList $ zip (s n) [eval (F f) ks| ks <- k'] where
    n = (size o) - 1
    k' = map ((&) o) (embed n)
