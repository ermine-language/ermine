module Syntax.List where

import Control.Monad
import List

infixl 4 >>= >> << <*> <$ <$>

return = pure listAp

(>>=) = bind listMonad

map : forall a b. (a -> b) -> List a -> List b
map = fmap listFunctor

(>>) : forall a b. List a -> List b -> List b
(>>) m n = m >>= _ -> n

(<<) : forall a b. List a -> List b -> List a
(<<) m n = m >>= a -> map (_ -> a) n

(<*>) : forall a b. List (a -> b) -> List a -> List b
(<*>) = ap listAp

(<$) : forall a b. a -> List b -> List a
(<$) a mb = map (_ -> a) mb

(<$>) : forall a b. (a -> b) -> List a -> List b
(<$>) = map
