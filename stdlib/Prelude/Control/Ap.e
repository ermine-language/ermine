module Control.Ap where

export Control.Functor

data Ap f = Ap
  (forall a. a -> f a)
  (forall a b. f (a -> b) -> f a -> f b)

pure : Ap f -> a -> f a
pure (Ap p f) a = p a

ap : Ap f -> f (a -> b) -> f a -> f b
ap (Ap p f) mf ma = f mf ma

liftA : Ap f -> (a -> b) -> f a -> f b
liftA f = fmap (apFunctor f)

liftA2 : Ap f -> (a -> b -> c) -> f a -> f b -> f c
liftA2 f g fa fb = ap f (ap f (pure f g) fa) fb

liftA3 : Ap f -> (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f g fa fb fc = ap f (ap f (ap f (pure f g) fa) fb) fc

liftA4 : Ap f -> (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f g fa fb fc fd = ap f (ap f (ap f (ap f (pure f g) fa) fb) fc) fd

liftA5 : Ap f -> (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f g fa fb fc fd fe = ap f (ap f (ap f (ap f (ap f (pure f g) fa) fb) fc) fd) fe

apFunctor: Ap f -> Functor f
apFunctor (Ap p ap) = Functor (f -> ap (p f))

tupleA2 : Ap f -> f a -> f b -> f (a, b)
tupleA2 f = liftA2 f (a b -> (a, b))
