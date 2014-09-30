module Syntax.Either where

import Control.Monad
import Either

infixl 4 >>= >> << <*> <$ <$>
infixl 3 |

return = pure eitherAp

(>>=) = bind eitherMonad

map : forall e a b. (a -> b) -> Either e a -> Either e b
map = fmap eitherFunctor

(>>) : forall e a b. Either e a -> Either e b -> Either e b
(>>) m n = m >>= _ -> n

(<<) : forall e a b. Either e a -> Either e b -> Either e a
(<<) m n = m >>= a -> map (_ -> a) n

(<*>) : forall e a b. Either e (a -> b) -> Either e a -> Either e b
(<*>) = ap eitherAp

(<$) : forall e a b. a -> Either e b -> Either e a
(<$) a mb = map (_ -> a) mb

(<$>) : forall e a b. (a -> b) -> Either e a -> Either e b
(<$>) = map

(|) : forall e a b. Either e a -> Either e a -> Either e a
(|) (Right e) a = a
(|) a         _ = a
