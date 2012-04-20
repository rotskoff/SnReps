-- Module	: Group
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

module Group where


-- The minimal group class
-- Note that the multiplication operator is not necessary commutative!

class (Eq a) => Group a where
   id  :: Int -> a
   (&) :: a -> a -> a
   inv :: a -> a
