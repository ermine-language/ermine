module Syntax.Do where

{- This module provides functions to support the `do` Ermine syntax.
For example, this expression:

    do x  <- unit "hi"
       x2 <- get
       put 42
       unit (x, x2)

with these auxiliary definitions:

    get : Monad (State s) -> State s s
    put : s -> Monad (State s) -> State s ()

gets desugared to

    bind (unit "hi")
         (x -> bind get
                    (x2 -> bind (put 42)
                                (_ -> unit (x, x2))))
    : Monad (State Int) -> State Int a

Note that (forall a. State a) is type-derived from our use of `get',
and (State Int) is derived from put 42.  A more generic definition:

    liftM2 : (a -> b -> c)
             -> (Monad f -> f a) -> (Monad f -> f b)
             -> Monad f -> f c
    liftM2 f mx my = do
      x <- mx
      y <- my
      unit (f x y)

Effectively, our monadic values are all lifted into a reader of their
monad vtable.
-}

import Control.Monad as M
import Function using const; flip

private type MonadK f a = (Monad_M f) -> f a

-- Wrap a pure value in any monad, after reading that monad instance.
unit : a -> MonadK f a
unit = flip unit_M

-- Monad bind of monad-instance-reading values.  Used for desugaring
-- do-expressions.
bind : MonadK f a -> (a -> MonadK f b) -> MonadK f b
bind fa f m = bind_M m (fa m) (flip f m)

-- Lift a monadic value to be usable in `do'.
liftDo : f a -> MonadK f a
liftDo = const
