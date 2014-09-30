module Num where

import Primitive as P
import Ord using {type Ord; fromLess}

infix 4 < > <= >=
infixl 5 + -
infixl 6 * /

(+), (-), (*), (/), pow : Num n => n -> n -> n
(+) = (+_P)
(-) = (-_P)
(*) = (*_P)
(/) = (/_P)
pow = pow_P

neg, abs : Num n => n -> n
neg = neg_P
abs = abs_P

toInt : Num n => n -> Int
toInt = primToInt#

toDouble : Num n => n -> Double
toDouble = primToDouble#

numOrd : Num n => Ord n
numOrd = fromLess (<)

nan : Double
nan = 0.0/0.0

-- fake reexport
(<), (<=), (>), (>=) : Primitive a => a -> a -> Bool
(<) = (<_P)
(<=) = (<=_P)
(>) = (>_P)
(>=) = (>=_P)
