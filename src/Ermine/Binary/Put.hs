--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Binary.Put
  ( MonadPut(..)
  ) where

import qualified Data.Binary.Put as B
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import qualified Data.Serialize.Put as S
import Data.Word

class Monad m => MonadPut m where
  putWord8 :: Word8 -> m ()
  putByteString     :: Strict.ByteString -> m ()
  putLazyByteString :: Lazy.ByteString -> m ()
  flush :: m ()
  putWord16le   :: Word16 -> m ()
  putWord16be   :: Word16 -> m ()
  putWord16host :: Word16 -> m ()
  putWord32le   :: Word32 -> m ()
  putWord32be   :: Word32 -> m ()
  putWord32host :: Word32 -> m ()
  putWord64le   :: Word64 -> m ()
  putWord64be   :: Word64 -> m ()
  putWord64host :: Word64 -> m ()
  putWordhost   :: Word   -> m ()

instance MonadPut B.PutM where
  putWord8 = B.putWord8
  putByteString = B.putByteString
  putLazyByteString = B.putLazyByteString
  flush = B.flush
  putWord16le   = B.putWord16le
  putWord16be   = B.putWord16be
  putWord16host = B.putWord16host
  putWord32le   = B.putWord32le
  putWord32be   = B.putWord32be
  putWord32host = B.putWord32host
  putWord64le   = B.putWord64le
  putWord64be   = B.putWord64be
  putWord64host = B.putWord64host
  putWordhost   = B.putWordhost

instance MonadPut S.PutM where
  putWord8 = S.putWord8
  putByteString = S.putByteString
  putLazyByteString = S.putLazyByteString
  flush = S.flush
  putWord16le   = S.putWord16le
  putWord16be   = S.putWord16be
  putWord16host = S.putWord16host
  putWord32le   = S.putWord32le
  putWord32be   = S.putWord32be
  putWord32host = S.putWord32host
  putWord64le   = S.putWord64le
  putWord64be   = S.putWord64be
  putWord64host = S.putWord64host
  putWordhost   = S.putWordhost
