module Ring where

import Function
import Num

data Ring a = Ring (a -> a -> a) (a -> a) (a -> a -> a) (Int -> a)

add : Ring a -> a -> a -> a
add (Ring p _ _ _) x y = p x y

negate : Ring a -> a -> a
negate (Ring _ n _ _) x = n x

subtract : Ring a -> a -> a -> a
subtract n a b = add n a (negate n b)

times : Ring a -> a -> a -> a
times (Ring _ _ m _) x y = m x y

fromInt : Ring a -> Int -> a
fromInt (Ring _ _ _ f) a = f a

zero r = fromInt r 0
one r = fromInt r 1

intRing = Ring (+) neg (*) id
doubleRing = Ring (+) neg (*) toDouble
