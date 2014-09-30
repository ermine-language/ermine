module Eq where

{-
class Eq a

infix 4 ==

(==) :: Eq a => a -> a -> Bool
(==) = eq#

instance Eq Int
instance Eq ()
instance Eq (a,b) | Eq a, Eq b
instance Eq
-}
