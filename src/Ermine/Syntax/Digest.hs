{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Syntax.Digest
-- Copyright :  (c) Edward Kmett 2011-12
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Digest
  (
  -- * Constructing Digests
    Digestable(..)
  , Digestable1(..)
  ) where

import Bound
import Crypto.Classes
import Data.Binary
import Data.Binary.Builder
import Data.Binary.Put
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text as SText
import qualified Data.Text.Encoding as SText
import qualified Data.Text.Lazy as LText
import Data.Int
import Data.Void
import GHC.Generics
import Prelude.Extras

nullBS :: Strict.ByteString
nullBS = Strict.pack [0]


--------------------------------------------------------------------
-- MD5
--------------------------------------------------------------------

class Digestable t where
  -- | update a hash. This will default to updating using 'show' if not specified
  digest :: Hash ctx d => ctx -> t -> ctx
  default digest :: (Hash ctx d, Generic t, GDigestable (Rep t)) => ctx -> t -> ctx
  digest c = gdigest c . from

  digestList :: Hash ctx d => ctx -> [t] -> ctx
  digestList = Prelude.foldr (\e c -> updateCtx (digest c e) nullBS)

class Digestable1 t where
  digest1 :: (Hash ctx d, Digestable a) => ctx -> t a -> ctx
  default digest1 :: (Hash ctx d, Digestable (t a)) => ctx -> t a -> ctx
  digest1 = digest

instance (Digestable b, Digestable v) => Digestable (Var b v) where

instance (Digestable1 f, Digestable a) => Digestable (Lift1 f a) where
  digest c (Lift1 x) = digest1 c x

instance (Digestable b, Digestable1 f, Functor f, Digestable v) => Digestable (Scope b f v) where
  digest c (Scope e) = digest1 c $ (fmap . fmap) Lift1 e

instance Digestable t => Digestable [t] where
  digest = digestList

instance Digestable Lazy.ByteString where
  digest c = Prelude.foldr (flip updateCtx) c . Lazy.toChunks

instance Digestable LText.Text where
  digest c = Prelude.foldr (flip updateCtx . SText.encodeUtf8) c
           . LText.toChunks

instance Digestable () where

digestBinary :: Binary b => Hash ctx d => ctx -> b -> ctx
digestBinary c = digest c . runPut . put

instance Digestable Int where digest = digestBinary
instance Digestable Int8 where digest = digestBinary
instance Digestable Int16 where digest = digestBinary
instance Digestable Int32 where digest = digestBinary
instance Digestable Int64 where digest = digestBinary
instance Digestable Word where digest = digestBinary
instance Digestable Word8 where digest = digestBinary
instance Digestable Word16 where digest = digestBinary
instance Digestable Word32 where digest = digestBinary
instance Digestable Word64 where digest = digestBinary
instance Digestable Float where digest = digestBinary
instance Digestable Double where digest = digestBinary
instance Digestable Integer where digest = digestBinary
instance Digestable Char where
  digest = digestBinary
  digestList c xs = updateCtx c (UTF8.fromString xs)

instance Digestable Strict.ByteString where
  digest = updateCtx

instance Digestable SText.Text where
  digest c = updateCtx c . SText.encodeUtf8

instance Digestable Builder where
  digest c = digest c . toLazyByteString

instance Digestable Void where

class GDigestable f where
  gdigest :: Hash cxt d => cxt -> f a -> cxt

instance GDigestable V1 where
  gdigest _ = error "Digesting void"

instance GDigestable U1 where
  gdigest c _ = c

instance (GDigestable f, GDigestable g) => GDigestable (f :*: g) where
  gdigest c (x :*: y) = c `gdigest` x `gdigest` y

instance (GDigestable f, GDigestable g) => GDigestable (f :+: g) where
  gdigest c (L1 x) = c `digest` Strict.singleton 1 `gdigest` x
  gdigest c (R1 y) = c `digest` Strict.singleton 2 `gdigest` y

instance GDigestable f => GDigestable (M1 i c f) where
  gdigest c (M1 x) = c `gdigest` x

instance Digestable a => GDigestable (K1 i a) where
  gdigest c (K1 x) = c `digest` x