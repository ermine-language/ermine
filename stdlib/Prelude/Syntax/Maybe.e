module Syntax.Maybe where

import Control.Monad
import Maybe

infixl 4 >>= >> << <*> <$ <$>
infixl 3 |

return = pure maybeAp

(>>=) = bind maybeMonad

map : forall a b. (a -> b) -> Maybe a -> Maybe b
map = fmap maybeFunctor

(>>) : forall a b. Maybe a -> Maybe b -> Maybe b
(>>) m n = m >>= _ -> n

(<<) : forall a b. Maybe a -> Maybe b -> Maybe a
(<<) m n = m >>= a -> map (_ -> a) n

(<*>) : forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
(<*>) = ap maybeAp

(<$) : forall a b. a -> Maybe b -> Maybe a
(<$) a mb = map (_ -> a) mb

(<$>) : forall a b. (a -> b) -> Maybe a -> Maybe b
(<$>) = map

(|) : forall a b. Maybe a -> Maybe a -> Maybe a
(|) Nothing a = a
(|) a       _ = a
