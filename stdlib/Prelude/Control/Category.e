module Control.Category where

import Control.Monoid

{- ^ Categories. -}

data Category k =
  Category (forall a. k a a) (forall a b c. k b c -> k a b -> k a c)

idC : Category k -> k a a
idC ~(Category id _) = id

comp : Category k -> k b c -> k a b -> k a c
comp ~(Category _ c) = c

catEndoMonoid : Category k -> Monoid (k a a)
catEndoMonoid (Category id comp) = Monoid id comp
