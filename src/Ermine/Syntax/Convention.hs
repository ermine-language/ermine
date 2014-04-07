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
module Ermine.Syntax.Convention
  ( Convention(..)
  , strict
  , conv
  , Conventional(..)
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

-- | calling conventions for applications and lambdas
data Convention
  = C -- lazy core appliction
  | U -- strict unboxed value
  | D -- strict dictionary application
  | N -- strict native value application (for strings and foreign java objects)
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Typeable,Data,Generic)

instance Hashable Convention where
  hashWithSalt n i = n `hashWithSalt` fromEnum i

instance Serial Convention where
  serialize = serialize . fromEnum
  deserialize = toEnum <$> deserialize

instance Binary Convention where
  put = serialize
  get = deserialize

instance Serialize Convention where
  put = serialize
  get = deserialize

strict :: Convention -> Bool
strict C = False
strict U = True
strict D = True
strict N = True

data Conventional a = Conventional a a a a
  deriving (Eq,Ord,Show,Read,Foldable,Traversable,Typeable,Data,Generic)

instance Num a => Num (Conventional a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap abs
  fromInteger = pure . fromInteger

instance Hashable a => Hashable (Conventional a)

instance Hashable1 Conventional

instance Serial1 Conventional where
  serializeWith f (Conventional a b c d) = f a >> f b >> f c >> f d
  deserializeWith f = Conventional <$> f <*> f <*> f <*> f

instance Serial a => Serial (Conventional a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary a => Binary (Conventional a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (Conventional a) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

instance Default a => Default (Conventional a) where
  def = Conventional def def def def

conv :: Convention -> Lens' (Conventional a) a
conv C = \ f (Conventional c u d n) -> f c <&> \c' -> Conventional c' u d n
conv U = \ f (Conventional c u d n) -> f u <&> \u' -> Conventional c u' d n
conv D = \ f (Conventional c u d n) -> f d <&> \d' -> Conventional c u d' n
conv N = \ f (Conventional c u d n) -> f n <&> \n' -> Conventional c u d n'

type instance Index (Conventional a) = Convention
type instance IxValue (Conventional a) = a

instance Ixed (Conventional a) where
  ix = conv

instance Representable Conventional where
  type Rep Conventional = Convention
  tabulate f = Conventional (f C) (f U) (f D) (f N)
  index (Conventional c _ _ _) C = c
  index (Conventional _ u _ _) U = u
  index (Conventional _ _ d _) D = d
  index (Conventional _ _ _ n) N = n

instance Functor Conventional where
  a <$ _ = Conventional a a a a
  fmap f (Conventional a b c d) = Conventional (f a) (f b) (f c) (f d)

instance Applicative Conventional where
  pure a = Conventional a a a a
  Conventional fa fb fc fd <*> Conventional aa ab ac ad = Conventional (fa aa) (fb ab) (fc ac) (fd ad)
  _ *> x = x
  x <* _ = x

instance Monad Conventional where
  return a = Conventional a a a a
  _ >> x = x
  Conventional a b c d >>= f = Conventional a' b' c' d' where
    Conventional a' _  _  _  = f a
    Conventional _  b' _  _  = f b
    Conventional _  _  c' _  = f c
    Conventional _  _  _  d' = f d

instance Distributive Conventional where
  distribute = distributeRep

instance MonadReader Convention Conventional where
  ask   = askRep
  local = localRep

instance MonadFix Conventional where
  mfix = mfixRep

instance MonadZip Conventional where
  mzip     = mzipRep
  mzipWith = mzipWithRep

instance FunctorWithIndex Convention Conventional where
  imap f (Conventional c u d n) = Conventional (f C c) (f U u) (f D d) (f N n)

instance FoldableWithIndex Convention Conventional where
  ifoldMap f (Conventional c u d n) = f C c <> f U u <> f D d <> f N n

instance TraversableWithIndex Convention Conventional where
  itraverse f (Conventional c u d n) = Conventional <$> f C c <*> f U u <*> f D d <*> f N n
