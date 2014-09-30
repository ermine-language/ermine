module Control.Monad where

import Bool
import Function
export Control.Ap

data Monad f = Monad (forall a. a -> f a) (forall a b. f a -> (a -> f b) -> f b)

unit : Monad f -> a -> f a
unit (Monad u b) = u

bind : Monad f -> f a -> (a -> f b) -> f b
bind (Monad u b) = b

join : Monad f -> f (f a) -> f a
join m ffa = bind m ffa id

liftM : Monad f -> (a -> b) -> f a -> f b
liftM f = liftA (monadAp f)

monadAp: Monad f -> Ap f
monadAp m = Ap (unit m) (mf ma -> bind m mf (f -> bind m ma (a -> unit m (f a))))

monadFunctor : Monad f -> Functor f
monadFunctor = apFunctor . monadAp

-- | If Q, run C's action and include its result; else, run A's action
-- and include its result.  Utterly incompatible with `liftA3 if`,
-- because that sequences all actions regardless of the boolean's
-- value.
ifM : Monad m -> m Bool -> m a -> m a -> m a
ifM m q c a = bind m q (b -> if b c a)
