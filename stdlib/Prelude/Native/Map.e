module Native.Map where

{-
builtin
  data Map (k: *) (v: *)
  insert : k -> v -> Map k v -> Map k v
  empty : Map k v
  union : Map k v -> Map k v -> Map k v
  size : Map k v -> Int
  lookup# : k -> Map k v -> Maybe# v
-}
