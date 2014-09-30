module List.NonEmpty where

import List as List

infixr 5 :| ++

data NonEmpty a = (:|) a (List a)

head (a :| _) = a
tail (_ :| as) = as
cons a (b :| bs) = a :| b :: bs

(++) xs ys = head xs :| tail xs ++_List head ys :: tail ys
