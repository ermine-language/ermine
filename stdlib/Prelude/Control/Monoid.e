module Control.Monoid where

data Monoid m = Monoid m (m -> m -> m)

mempty : Monoid m -> m
mempty (Monoid m _) = m

mappend : Monoid m -> m -> m -> m
mappend (Monoid _ p) = p

mproduct : Monoid m -> Monoid n -> Monoid (m, n)
mproduct m n = Monoid (mempty m, mempty n)
                      (~(m1, n1) ~(m2, n2) -> (mappend m m1 m2, mappend n n1 n2))

-- sum = Monoid 0 (+)
-- product = Monoid 1 (*)
