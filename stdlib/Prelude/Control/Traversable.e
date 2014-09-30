module Control.Traversable where

import Control.Monad
import Function

type Traversal t = forall f a b. Ap f -> (a -> f b) -> t a -> f (t b)
data Traversable (t : * -> *) = Traversable (Traversal t)

traverse : Traversable t -> Ap f -> (a -> f b) -> t a -> f (t b)
traverse (Traversable trav) = trav

mapM : Traversable t -> Monad f -> (a -> f b) -> t a -> f (t b)
mapM t m = traverse t (monadAp m)

sequenceA : Traversable t -> Ap f -> t (f a) -> f (t a)
sequenceA t a = traverse t a id

sequence : Traversable t -> Monad f -> t (f a) -> f (t a)
sequence t a = mapM t a id
