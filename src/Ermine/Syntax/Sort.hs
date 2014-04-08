{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Sort
  ( Sort(..)
  , sort
  , Sorted(..)
  ) where

import Control.Applicative
import Control.Lens hiding (strict)
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.Zip
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bytes.Serial
import Data.Data
import Data.Default
import Data.Distributive
import Data.Foldable
import Data.Functor.Rep
import Data.Hashable
import Data.Hashable.Extras
import Data.Monoid
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import GHC.Generics hiding (Rep)
import Prelude

-- | calling sortentions for applications and lambdas
data Sort
  = B -- boxed
  | U -- unboxed
  | N -- native
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Typeable,Data,Generic)

instance Hashable Sort where
  hashWithSalt n i = n `hashWithSalt` fromEnum i

instance Serial Sort where
  serialize = serialize . fromEnum
  deserialize = toEnum <$> deserialize

instance Binary Sort where
  put = serialize
  get = deserialize

instance Serialize Sort where
  put = serialize
  get = deserialize

data Sorted a = Sorted a a a
  deriving (Eq,Ord,Show,Read,Foldable,Traversable,Typeable,Data,Generic)

instance Num a => Num (Sorted a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap abs
  fromInteger = pure . fromInteger

instance Hashable a => Hashable (Sorted a)

instance Hashable1 Sorted

instance Serial1 Sorted where
  serializeWith f (Sorted a b c) = f a >> f b >> f c
  deserializeWith f = Sorted <$> f <*> f <*> f

instance Serial a => Serial (Sorted a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary a => Binary (Sorted a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (Sorted a) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

instance Default a => Default (Sorted a) where
  def = Sorted def def def

sort :: Sort -> Lens' (Sorted a) a
sort B = \ f (Sorted b u n) -> f b <&> \b' -> Sorted b' u n
sort U = \ f (Sorted b u n) -> f u <&> \u' -> Sorted b u' n
sort N = \ f (Sorted b u n) -> f n <&> \n' -> Sorted b u n'

type instance Index (Sorted a) = Sort
type instance IxValue (Sorted a) = a

instance Ixed (Sorted a) where
  ix = sort

instance Representable Sorted where
  type Rep Sorted = Sort
  tabulate f = Sorted (f B) (f U) (f N)
  index (Sorted c _ _) B = c
  index (Sorted _ u _) U = u
  index (Sorted _ _ n) N = n

instance Functor Sorted where
  a <$ _ = Sorted a a a
  fmap f (Sorted a b c) = Sorted (f a) (f b) (f c)

instance Applicative Sorted where
  pure a = Sorted a a a
  Sorted fa fb fc <*> Sorted aa ab ac = Sorted (fa aa) (fb ab) (fc ac)
  _ *> x = x
  x <* _ = x

instance Monad Sorted where
  return a = Sorted a a a
  _ >> x = x
  Sorted a b c >>= f = Sorted a' b' c' where
    Sorted a' _  _  = f a
    Sorted _  b' _  = f b
    Sorted _  _  c' = f c

instance Distributive Sorted where
  distribute = distributeRep

instance MonadReader Sort Sorted where
  ask   = askRep
  local = localRep

instance MonadFix Sorted where
  mfix = mfixRep

instance MonadZip Sorted where
  mzip     = mzipRep
  mzipWith = mzipWithRep

instance FunctorWithIndex Sort Sorted where
  imap f (Sorted b u n) = Sorted (f B b) (f U u) (f N n)

instance FoldableWithIndex Sort Sorted where
  ifoldMap f (Sorted b u n) = f B b <> f U u <> f N n

instance TraversableWithIndex Sort Sorted where
  itraverse f (Sorted b u n) = Sorted <$> f B b <*> f U u <*> f N n
