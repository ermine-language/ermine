{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
  , DigestableList(..)
  ) where

import Control.Applicative
import Crypto.Classes
import Foreign
import Foreign.C
import Data.Data (Data, Typeable)
import GHC.IO (unsafeDupablePerformIO)
import Data.Foldable hiding (concatMap)
import Data.Default
import Data.Binary.Builder
import Data.Hashable
import qualified Data.ByteString.Unsafe as Strict
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.UTF8 as UTF8


--------------------------------------------------------------------
-- MD5List
--------------------------------------------------------------------

-- | A helper class provided to aid in hash calculation
class DigestableList t where
  digestList :: Hash ctx d => ctx -> [t] -> ctx

nullBS :: Strict.ByteString
nullBS = Strict.pack [0]

instance DigestableList Strict.ByteString where
  digestList = Prelude.foldr (\xs c -> updateCtx (updateCtx c xs) nullBS)

instance DigestableList Char where
  digestList c xs = updateCtx c (UTF8.fromString xs)

--------------------------------------------------------------------
-- MD5
--------------------------------------------------------------------

class Digestable t where
  -- | update a hash. This will default to updating using 'show' if not specified
  digest :: Hash ctx d => ctx -> t -> ctx
  default digest :: (Hash ctx d, Show t) => ctx -> t -> ctx
  digest c = digest c . show

instance DigestableList t => Digestable [t] where
  digest = digestList

instance Digestable Lazy.ByteString where
  digest c = Prelude.foldr (flip updateCtx) c . Lazy.toChunks

instance Digestable () where
  digest c _ = c

instance Digestable Strict.ByteString where
  digest = updateCtx

instance Digestable Builder where
  digest c = digest c . toLazyByteString
