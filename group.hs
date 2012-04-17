module Group where


-- The minimal group class
-- Note that the multiplication operator is not necessary commutative!

class (Eq a) => Group a where
   id  :: Int -> a
   (&) :: a -> a -> a
   inv :: a -> a