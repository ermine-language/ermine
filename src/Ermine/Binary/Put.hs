{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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

import Control.Monad.Reader
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import qualified Data.Binary.Put as B
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import qualified Data.Serialize.Put as S
import Data.Word

class Monad m => MonadPut m where
  putWord8 :: Word8 -> m ()
  default putWord8 :: (m ~ t n, MonadTrans t, MonadPut n) => Word8 -> m ()
  putWord8 = lift . putWord8
  {-# INLINE putWord8 #-}

  putByteString     :: Strict.ByteString -> m ()
  default putByteString :: (m ~ t n, MonadTrans t, MonadPut n) => Strict.ByteString -> m ()
  putByteString = lift . putByteString
  {-# INLINE putByteString #-}

  putLazyByteString :: Lazy.ByteString -> m ()
  default putLazyByteString :: (m ~ t n, MonadTrans t, MonadPut n) => Lazy.ByteString -> m ()
  putLazyByteString = lift . putLazyByteString
  {-# INLINE putLazyByteString #-}

  flush :: m ()
  default flush :: (m ~ t n, MonadTrans t, MonadPut n) => m ()
  flush = lift flush
  {-# INLINE flush #-}

  putWord16le   :: Word16 -> m ()
  default putWord16le :: (m ~ t n, MonadTrans t, MonadPut n) => Word16 -> m ()
  putWord16le = lift . putWord16le
  {-# INLINE putWord16le #-}

  putWord16be   :: Word16 -> m ()
  default putWord16be :: (m ~ t n, MonadTrans t, MonadPut n) => Word16 -> m ()
  putWord16be = lift . putWord16be
  {-# INLINE putWord16be #-}

  putWord16host :: Word16 -> m ()
  default putWord16host :: (m ~ t n, MonadTrans t, MonadPut n) => Word16 -> m ()
  putWord16host = lift . putWord16host
  {-# INLINE putWord16host #-}

  putWord32le   :: Word32 -> m ()
  default putWord32le :: (m ~ t n, MonadTrans t, MonadPut n) => Word32 -> m ()
  putWord32le = lift . putWord32le
  {-# INLINE putWord32le #-}

  putWord32be   :: Word32 -> m ()
  default putWord32be :: (m ~ t n, MonadTrans t, MonadPut n) => Word32 -> m ()
  putWord32be = lift . putWord32be
  {-# INLINE putWord32be #-}

  putWord32host :: Word32 -> m ()
  default putWord32host :: (m ~ t n, MonadTrans t, MonadPut n) => Word32 -> m ()
  putWord32host = lift . putWord32host
  {-# INLINE putWord32host #-}

  putWord64le   :: Word64 -> m ()
  default putWord64le :: (m ~ t n, MonadTrans t, MonadPut n) => Word64 -> m ()
  putWord64le = lift . putWord64le
  {-# INLINE putWord64le #-}

  putWord64be   :: Word64 -> m ()
  default putWord64be :: (m ~ t n, MonadTrans t, MonadPut n) => Word64 -> m ()
  putWord64be = lift . putWord64be
  {-# INLINE putWord64be #-}

  putWord64host :: Word64 -> m ()
  default putWord64host :: (m ~ t n, MonadTrans t, MonadPut n) => Word64 -> m ()
  putWord64host = lift . putWord64host
  {-# INLINE putWord64host #-}

  putWordhost   :: Word   -> m ()
  default putWordhost :: (m ~ t n, MonadTrans t, MonadPut n) => Word -> m ()
  putWordhost = lift . putWordhost
  {-# INLINE putWordhost #-}

instance MonadPut B.PutM where
  putWord8 = B.putWord8
  {-# INLINE putWord8 #-}
  putByteString = B.putByteString
  {-# INLINE putByteString #-}
  putLazyByteString = B.putLazyByteString
  {-# INLINE putLazyByteString #-}
  flush = B.flush
  {-# INLINE flush #-}
  putWord16le   = B.putWord16le
  {-# INLINE putWord16le #-}
  putWord16be   = B.putWord16be
  {-# INLINE putWord16be #-}
  putWord16host = B.putWord16host
  {-# INLINE putWord16host #-}
  putWord32le   = B.putWord32le
  {-# INLINE putWord32le #-}
  putWord32be   = B.putWord32be
  {-# INLINE putWord32be #-}
  putWord32host = B.putWord32host
  {-# INLINE putWord32host #-}
  putWord64le   = B.putWord64le
  {-# INLINE putWord64le #-}
  putWord64be   = B.putWord64be
  {-# INLINE putWord64be #-}
  putWord64host = B.putWord64host
  {-# INLINE putWord64host #-}
  putWordhost   = B.putWordhost
  {-# INLINE putWordhost #-}

instance MonadPut S.PutM where
  putWord8 = S.putWord8
  {-# INLINE putWord8 #-}
  putByteString = S.putByteString
  {-# INLINE putByteString #-}
  putLazyByteString = S.putLazyByteString
  {-# INLINE putLazyByteString #-}
  flush = S.flush
  {-# INLINE flush #-}
  putWord16le   = S.putWord16le
  {-# INLINE putWord16le #-}
  putWord16be   = S.putWord16be
  {-# INLINE putWord16be #-}
  putWord16host = S.putWord16host
  {-# INLINE putWord16host #-}
  putWord32le   = S.putWord32le
  {-# INLINE putWord32le #-}
  putWord32be   = S.putWord32be
  {-# INLINE putWord32be #-}
  putWord32host = S.putWord32host
  {-# INLINE putWord32host #-}
  putWord64le   = S.putWord64le
  {-# INLINE putWord64le #-}
  putWord64be   = S.putWord64be
  {-# INLINE putWord64be #-}
  putWord64host = S.putWord64host
  {-# INLINE putWord64host #-}
  putWordhost   = S.putWordhost
  {-# INLINE putWordhost #-}

instance MonadPut m => MonadPut (Lazy.StateT s m)
instance MonadPut m => MonadPut (Strict.StateT s m)
instance MonadPut m => MonadPut (ReaderT e m)
instance (MonadPut m, Monoid w) => MonadPut (Lazy.WriterT w m)
instance (MonadPut m, Monoid w) => MonadPut (Strict.WriterT w m)
