module Function.Endo where

import Control.Monoid

type Endo a = a -> a

-- | Each category gives rise to an endomorphism monoid, including
-- this one.  As with Haskell's, mappend is (.).
endoMonoid : Monoid (Endo a)
endoMonoid = Monoid (a -> a) (f g x -> f (g x))
