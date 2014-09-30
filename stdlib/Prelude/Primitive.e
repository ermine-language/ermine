module Primitive where

{- These functions are slightly more general than those in Num, in
that they support the Nullable numbers.  For that reason, we lack
toInt and toDouble here; lift that into the Nullable functor if you
need it. -}

import Ord using fromLess

infix 4 < > <= >=
infixl 5 + -
infixl 6 * / %

(+), (-), (*), (/), pow, (%) : PrimitiveNum n => n -> n -> n
(+) = primPlus#
(-) = primMinus#
(*) = primTimes#
(/) = primDiv#
pow = primPow#
(%) = primMod#

neg, abs : PrimitiveNum n => n -> n
neg = primNeg#
abs = primAbs#

primOrd = fromLess (<)

(<), (<=), (>), (>=) : Primitive a => a -> a -> Bool
(<) = primLt#
(<=) = primLte#
(>) = primGt#
(>=) = primGte#
