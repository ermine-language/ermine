
module Data.Nu where

import Control.Functor

data Nu f = forall s. Unfold (s -> f s) s

unfold : (s -> f s) -> s -> Nu f
unfold = Unfold

observe : Functor f -> Nu f -> f (Nu f)
observe f (Unfold step seed) = fmap f (Unfold step) (step seed)

