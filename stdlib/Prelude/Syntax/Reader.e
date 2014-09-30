module Syntax.Reader where

import Control.Monad
import Control.Monad.Reader
import Function

infixl 4 >>= >> << <*> <$ <$> <$$>

return = pure readerAp

(>>=) = bind readerMonad

map : forall e a b. (a -> b) -> Reader e a -> Reader e b
map = fmap readerFunctor

lift2 = liftA2 readerAp
lift3 = liftA3 readerAp

(>>) : forall e a b. Reader e a -> Reader e b -> Reader e b
(>>) m n = m >>= _ -> n

(<<) : forall e a b. Reader e a -> Reader e b -> Reader e a
(<<) m n = m >>= a -> map (_ -> a) n

(<*>) : forall e a b. Reader e (a -> b) -> Reader e a -> Reader e b
(<*>) = ap readerAp

(<$) : forall e a b. a -> Reader e b -> Reader e a
(<$) a mb = map (_ -> a) mb

(<$>) : forall e a b. (a -> b) -> Reader e a -> Reader e b
(<$>) = map

-- useful for attaining a sort of forward chaining style;
-- which is useful in aiding the typechecker
-- named by analogy to <**>
(<$$>) : forall e a b . Reader e a -> (a -> b) -> Reader e b
(<$$>) = flip map


--liftA2 f x y = f <$> x <*> y
