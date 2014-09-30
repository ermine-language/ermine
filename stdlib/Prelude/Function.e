module Function where

import Control.Category
export Function.Endo

infixr 9 . |
infixl 0 $ $!

fCategory : Category (->)
fCategory = Category id (.)

flip : (a -> b -> c) -> b -> a -> c
flip f x y = f y x

(.) : (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

(|) : (a -> b) -> (b -> c) -> a -> c
(|) f g x = g (f x)

infixl 0 '
(') f a = f a

infixl 0 `
(`) a f = f a

id : a -> a
id x = x

curry : ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f ~(a,b) = f a b

($), ($!) : (a -> b) -> a -> b
($) a b = a b
($!) a !b = a b

infixl 5 &&&
(&&&) : (a -> b) -> (a -> c) -> (a -> (b,c))
(&&&) f g a = (f a, g a)

infixl 1 |>
(|>) x f = f x

infixl 2 <|
(<|) f x = f x

seq : a -> b -> b
seq !a b = b

const : a -> b -> a
const x _ = x

-- | Identity fixpoint.
fix : (a -> a) -> a
fix f = let x = f x in x

-- infixify functions
-- e.g. x -| mod |- n
-- or 0 -| foldl (+) |- [1,2,3]
infixl 9 -| |-
(-|) = flip ($)
(|-) = ($)
