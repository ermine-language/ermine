module Data.Cofree where

import Control.Comonad

infixr 5 :<
data Cofree f a = (:<) a (f (Cofree f a))

private
  mapCofree ff f (a :< as) = f a :< fmap ff (mapCofree ff f) as
  extendCofree ff f aas@(_ :< as) = f aas :< fmap ff (extendCofree ff f) as

cofreeFunctor f = Functor (mapCofree f)
cofreeComonad f = Comonad ((a :< _) -> a) (extendCofree f)
