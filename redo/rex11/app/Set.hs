module Set where
import Data.List
    
newtype Set a = Set {unSet :: [a]}
-- set union
(+*) :: (Eq a) => Set a -> Set a -> Set a
(Set (x:xs)) +* (Set ys) =
    if x `elem` ys then
        Set xs +* Set ys  -- keep elements unique, unlike list
    else
        Set xs +* Set (x:ys)
(Set []) +* (Set ys) = Set ys
-- set minus
(-*) :: (Eq a) => Set a -> Set a -> Set a
(Set xs) -* (Set []) = Set xs
(Set xs) -* (Set (y : ys)) = Set (delete y xs) -* Set ys
-- not set inclusion
notIn :: Eq a => a -> Set a -> Bool
notIn x (Set xs) = x `notElem` xs
-- create singleton set
set :: a -> Set a
set x = Set [x]

emptySet :: Set a
emptySet = Set []