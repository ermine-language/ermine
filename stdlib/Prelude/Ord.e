module Ord where

import Control.Monoid
import Bool
import Function

data Ordering = LT | EQ | GT

orderingMonoid : Monoid Ordering
orderingMonoid = Monoid EQ (a b -> case a of EQ -> b; _ -> a)

type Ord a = a -> a -> Ordering

ordMonoid : Monoid (Ord a)
ordMonoid = Monoid (const . const $ EQ)
                   (fa fb l r -> mappend orderingMonoid (fa l r) (fb l r))

eq,lt,gt,neq,ge,le : (a -> a -> Ordering) -> a -> a -> Bool
eq f a b = case f a b of EQ -> True; _ -> False
lt f a b = case f a b of LT -> True; _ -> False
gt f a b = case f a b of GT -> True; _ -> False
neq f a b = case f a b of EQ -> False; _ -> True
ge f a b = case f a b of LT -> False; _ -> True
le f a b = case f a b of GT -> False; _ -> True

min, max : (a -> a -> Ordering) -> a -> a -> a
min f a b = case f a b of GT -> b; _ -> a
max f a b = case f a b of LT -> b; _ -> a

argmin, argmax : (b -> b -> Ordering) -> (a -> b) -> a -> a -> a
argmin f g a b = case f (g a) (g b) of GT -> b; _ -> a
argmax f g a b = case f (g a) (g b) of LT -> b; _ -> a

nullableOrd : (a -> a -> Ordering) -> Nullable a -> Nullable a -> Ordering
nullableOrd _ (Null _) (Null _) = EQ
nullableOrd _ (Null _) _        = LT
nullableOrd _ _        (Null _) = GT
nullableOrd f (Some a) (Some b) = f a b

ord_Bool False True = LT
ord_Bool True False = GT
ord_Bool _ _ = EQ

contramap : (b -> a) -> Ord a -> Ord b
contramap f o bl br = o (f bl) (f br)

fromLess : (a -> a -> Bool) -> Ord a
fromLess aLt l r = case (aLt l r, aLt r l) of
    (True, _) -> LT
    (_, True) -> GT
    _         -> EQ

fromCompare : (a -> a -> Int) -> Ord a
fromCompare f a1 a2 =
  -- note: using unsafe builtin versions
  -- rather than versions in Primitive to avoid
  -- circular dependency Primitive <=> Ord
  let c = f a1 a2
  in if (primLt# c 0) LT (if (primGt# c 0) GT EQ)
