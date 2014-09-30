module Native.Pair where

-- builtin
--   data Pair# a b
--   toPair# : (a,b) -> Pair# a b
--   fst# : Pair# a b -> a
--   snd# : Pair# a b -> b
--   fromPair# : Pair# a b -> (a, b)

pair# : a -> b -> Pair# a b
pair# a b = toPair# (a, b)
