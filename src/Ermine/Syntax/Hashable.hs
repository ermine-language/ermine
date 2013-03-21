{-# LANGUAGE DefaultSignatures #-}
module Ermine.Syntax.Hashable 
  ( Hashable1(..)
  , Hashable2(..)
  ) where

import Data.Functor.Identity
import Data.Hashable

data Salt = Salt

instance Hashable Salt where
  hashWithSalt = const

-- this lets us extract the default salt used by Data.Hashable 1.2
salt :: Int
salt = hash Salt

class Hashable1 t where
  hashWithSalt1 :: Hashable a => Int -> t a -> Int
  default hashWithSalt1 :: (Hashable a, Hashable (t a)) => Int -> t a -> Int
  hashWithSalt1 = hashWithSalt1

  hash1 :: Hashable a => t a -> Int
  hash1 = hashWithSalt1 salt

instance Hashable1 Identity where
  hashWithSalt1 n (Identity a) = hashWithSalt n a

instance Hashable1 Maybe
instance Hashable1 []
instance Hashable a => Hashable1 (Either a)
instance Hashable a => Hashable1 ((,) a)
instance (Hashable a, Hashable b) => Hashable1 ((,,) a b)
instance (Hashable a, Hashable b, Hashable c) => Hashable1 ((,,,) a b c)
instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable1 ((,,,,) a b c d)
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable1 ((,,,,,) a b c d e)
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable1 ((,,,,,,) a b c d e f)

class Hashable2 t where
  hashWithSalt2 :: (Hashable a, Hashable b) => Int -> t a b -> Int
  default hashWithSalt2 :: (Hashable a, Hashable b, Hashable (t a b)) => Int -> t a b -> Int
  hashWithSalt2 = hashWithSalt

  hash2 :: (Hashable a, Hashable b) => t a b -> Int
  hash2 = hashWithSalt2 salt

instance Hashable2 Either
instance Hashable2 (,)
instance Hashable a => Hashable2 ((,,) a)
instance (Hashable a, Hashable b) => Hashable2 ((,,,) a b)
instance (Hashable a, Hashable b, Hashable c) => Hashable2 ((,,,,) a b c)
instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable2 ((,,,,,) a b c d)
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable2 ((,,,,,,) a b c d e)
