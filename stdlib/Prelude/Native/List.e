module Native.List where

import List

toList# : List a -> List# a
toList# = foldr (::#) Nil#

-- builtin
--   data List# (a: *)
--   (::#) : a -> List# a -> List# a
--   Nil : List# a
--   fromList# : List# a -> List a
--   head# : List# a -> a
--   tail# : List# a -> List# a
--   mkRelation# : List# {..a} -> [..a]
