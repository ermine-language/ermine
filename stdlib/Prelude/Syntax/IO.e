module Syntax.IO where

import Control.Monad
import IO

infixl 4 >>= >> << <*> <$ <$>

return = pure ioAp

(>>=) = bind ioMonad

map : forall a b. (a -> b) -> IO a -> IO b
map = fmap ioFunctor

(>>) : forall a b. IO a -> IO b -> IO b
(>>) m n = m >>= _ -> n

(<<) : forall a b. IO a -> IO b -> IO a
(<<) m n = m >>= a -> map (_ -> a) n

(<*>) : forall a b. IO (a -> b) -> IO a -> IO b
(<*>) = ap ioAp

(<$) : forall a b. a -> IO b -> IO a
(<$) a mb = map (_ -> a) mb

(<$>) : forall a b. (a -> b) -> IO a -> IO b
(<$>) = map
