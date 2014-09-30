module Bool where

infixr 3 &&
infixr 2 ||
infixr 1 ==>

(&&) : Bool -> Bool -> Bool
(&&) True a = a
(&&) False a = False

(||) : Bool -> Bool -> Bool
(||) True a = True
(||) False a = a

(==>) : Bool -> Bool -> Bool
(==>) True  a = a
(==>) False _ = True

not : Bool -> Bool
not True = False
not False = True

if : Bool -> a -> a -> a
if True x _ = x
if False _ y = y

otherwise : Bool
otherwise = True

