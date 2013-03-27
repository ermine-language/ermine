{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: type-families
--
--------------------------------------------------------------------
module Ermine.Binary.Get
  ( MonadGet(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.Int
import qualified Data.Serialize.Get as S
import Data.Word

class (Integral (Unchecked m), Monad m) => MonadGet m where
  type Unchecked m :: *
  type Bytes m :: *

  skip :: Int -> m ()
#ifndef HLINT
  default skip :: (MonadTrans t, MonadGet n, m ~ t n) => Int -> m ()
  skip = lift . skip
#endif

  uncheckedSkip :: Unchecked m -> m ()
#ifndef HLINT
  default uncheckedSkip :: (MonadTrans t, MonadGet n, m ~ t n) => Unchecked n -> m ()
  uncheckedSkip = lift . uncheckedSkip
#endif

  lookAhead :: m a -> m a
  lookAheadM :: m (Maybe a) -> m (Maybe a)
  lookAheadE :: m (Either a b) -> m (Either a b)

  uncheckedLookAhead :: Unchecked m -> m (Bytes m)
#ifndef HLINT
  default uncheckedLookAhead :: (MonadTrans t, MonadGet n, m ~ t n) => Unchecked n -> m (Bytes n)
  uncheckedLookAhead = lift . uncheckedLookAhead
#endif

  getBytes :: Int -> m Strict.ByteString
#ifndef HLINT
  default getBytes :: (MonadTrans t, MonadGet n, m ~ t n) => Int -> m Strict.ByteString
  getBytes = lift . getBytes
#endif

  remaining :: m (Unchecked m)
#ifndef HLINT
  default remaining :: (MonadTrans t, MonadGet n, m ~ t n) => m (Unchecked n)
  remaining = lift remaining
#endif

  isEmpty :: m Bool
#ifndef HLINT
  default isEmpty :: (MonadTrans t, MonadGet n, m ~ t n) => m Bool
  isEmpty = lift isEmpty
#endif

  getWord8 :: m Word8
#ifndef HLINT
  default getWord8 :: (MonadTrans t, MonadGet n, m ~ t n) => m Word8
  getWord8 = lift getWord8
#endif

  getByteString :: Int -> m Strict.ByteString
#ifndef HLINT
  default getByteString :: (MonadTrans t, MonadGet n, m ~ t n) => Int -> m Strict.ByteString
  getByteString = lift . getByteString
#endif

  getLazyByteString :: Int64 -> m Lazy.ByteString
#ifndef HLINT
  default getLazyByteString :: (MonadTrans t, MonadGet n, m ~ t n) => Int64 -> m Lazy.ByteString
  getLazyByteString = lift . getLazyByteString
#endif

  getWord16be   :: m Word16
#ifndef HLINT
  default getWord16be :: (MonadTrans t, MonadGet n, m ~ t n) => m Word16
  getWord16be = lift getWord16be
#endif

  getWord16le   :: m Word16
#ifndef HLINT
  default getWord16le :: (MonadTrans t, MonadGet n, m ~ t n) => m Word16
  getWord16le = lift getWord16le
#endif

  getWord16host :: m Word16
#ifndef HLINT
  default getWord16host :: (MonadTrans t, MonadGet n, m ~ t n) => m Word16
  getWord16host = lift getWord16host
#endif

  getWord32be   :: m Word32
#ifndef HLINT
  default getWord32be :: (MonadTrans t, MonadGet n, m ~ t n) => m Word32
  getWord32be = lift getWord32be
#endif

  getWord32le   :: m Word32
#ifndef HLINT
  default getWord32le :: (MonadTrans t, MonadGet n, m ~ t n) => m Word32
  getWord32le = lift getWord32le
#endif

  getWord32host :: m Word32
#ifndef HLINT
  default getWord32host :: (MonadTrans t, MonadGet n, m ~ t n) => m Word32
  getWord32host = lift getWord32host
#endif

  getWord64be   :: m Word64
#ifndef HLINT
  default getWord64be :: (MonadTrans t, MonadGet n, m ~ t n) => m Word64
  getWord64be = lift getWord64be
#endif

  getWord64le   :: m Word64
#ifndef HLINT
  default getWord64le :: (MonadTrans t, MonadGet n, m ~ t n) => m Word64
  getWord64le = lift getWord64le
#endif

  getWord64host :: m Word64
#ifndef HLINT
  default getWord64host :: (MonadTrans t, MonadGet n, m ~ t n) => m Word64
  getWord64host = lift getWord64host
#endif

  getWordhost :: m Word
#ifndef HLINT
  default getWordhost :: (MonadTrans t, MonadGet n, m ~ t n) => m Word
  getWordhost = lift getWordhost
#endif

instance MonadGet B.Get where
  type Unchecked B.Get = Int64
  type Bytes B.Get = Lazy.ByteString
  skip = B.skip
  {-# INLINE skip #-}
  uncheckedSkip = B.uncheckedSkip
  {-# INLINE uncheckedSkip #-}
  lookAhead = B.lookAhead
  {-# INLINE lookAhead #-}
  lookAheadM = B.lookAheadM
  {-# INLINE lookAheadM #-}
  lookAheadE = B.lookAheadE
  {-# INLINE lookAheadE #-}
  uncheckedLookAhead = B.uncheckedLookAhead
  {-# INLINE uncheckedLookAhead #-}
  getBytes = B.getBytes
  {-# INLINE getBytes #-}
  remaining = B.remaining
  {-# INLINE remaining #-}
  isEmpty = B.isEmpty
  {-# INLINE isEmpty #-}
  getWord8 = B.getWord8
  {-# INLINE getWord8 #-}
  getByteString = B.getByteString
  {-# INLINE getByteString #-}
  getLazyByteString = B.getLazyByteString
  {-# INLINE getLazyByteString #-}
  getWord16be   = B.getWord16be
  {-# INLINE getWord16be #-}
  getWord16le   = B.getWord16le
  {-# INLINE getWord16le #-}
  getWord16host = B.getWord16host
  {-# INLINE getWord16host #-}
  getWord32be   = B.getWord32be
  {-# INLINE getWord32be #-}
  getWord32le   = B.getWord32le
  {-# INLINE getWord32le #-}
  getWord32host = B.getWord32host
  {-# INLINE getWord32host #-}
  getWord64be   = B.getWord64be
  {-# INLINE getWord64be #-}
  getWord64le   = B.getWord64le
  {-# INLINE getWord64le #-}
  getWord64host = B.getWord64host
  {-# INLINE getWord64host #-}
  getWordhost   = B.getWordhost
  {-# INLINE getWordhost #-}

instance MonadGet S.Get where
  type Unchecked S.Get = Int
  type Bytes S.Get = Strict.ByteString
  skip = S.skip
  {-# INLINE skip #-}
  uncheckedSkip = S.uncheckedSkip
  {-# INLINE uncheckedSkip #-}
  lookAhead = S.lookAhead
  {-# INLINE lookAhead #-}
  lookAheadM = S.lookAheadM
  {-# INLINE lookAheadM #-}
  lookAheadE = S.lookAheadE
  {-# INLINE lookAheadE #-}
  uncheckedLookAhead = S.uncheckedLookAhead
  {-# INLINE uncheckedLookAhead #-}
  getBytes = S.getBytes
  {-# INLINE getBytes #-}
  remaining = S.remaining
  {-# INLINE remaining #-}
  isEmpty = S.isEmpty
  {-# INLINE isEmpty #-}
  getWord8 = S.getWord8
  {-# INLINE getWord8 #-}
  getByteString = S.getByteString
  {-# INLINE getByteString #-}
  getLazyByteString = S.getLazyByteString
  {-# INLINE getLazyByteString #-}
  getWord16be   = S.getWord16be
  {-# INLINE getWord16be #-}
  getWord16le   = S.getWord16le
  {-# INLINE getWord16le #-}
  getWord16host = S.getWord16host
  {-# INLINE getWord16host #-}
  getWord32be   = S.getWord32be
  {-# INLINE getWord32be #-}
  getWord32le   = S.getWord32le
  {-# INLINE getWord32le #-}
  getWord32host = S.getWord32host
  {-# INLINE getWord32host #-}
  getWord64be   = S.getWord64be
  {-# INLINE getWord64be #-}
  getWord64le   = S.getWord64le
  {-# INLINE getWord64le #-}
  getWord64host = S.getWord64host
  {-# INLINE getWord64host #-}
  getWordhost   = S.getWordhost
  {-# INLINE getWordhost #-}

instance MonadGet m => MonadGet (Lazy.StateT s m) where
  type Unchecked (Lazy.StateT s m) = Unchecked m
  type Bytes (Lazy.StateT s m) = Bytes m
  lookAhead (Lazy.StateT m) = Lazy.StateT (lookAhead . m)
  {-# INLINE lookAhead #-}
  lookAheadM (Lazy.StateT m) = Lazy.StateT (liftM factor . lookAheadE . liftM distribute . m)
    where
    distribute (Nothing, s') = Left (Nothing, s')
    distribute (Just a, s') = Right (Just a, s')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Lazy.StateT m) = Lazy.StateT (liftM factor . lookAheadE . liftM distribute . m)
    where
    distribute (Left a, s') = Left (Left a, s')
    distribute (Right b, s') = Right (Right b, s')
    factor = either id id
  {-# INLINE lookAheadE #-}

instance MonadGet m => MonadGet (Strict.StateT s m) where
  type Unchecked (Strict.StateT s m) = Unchecked m
  type Bytes (Strict.StateT s m) = Bytes m
  lookAhead (Strict.StateT m) = Strict.StateT (lookAhead . m)
  {-# INLINE lookAhead #-}
  lookAheadM (Strict.StateT m) = Strict.StateT (liftM factor . lookAheadE . liftM distribute . m)
    where
    distribute (Nothing, s') = Left (Nothing, s')
    distribute (Just a, s') = Right (Just a, s')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Strict.StateT m) = Strict.StateT (liftM factor . lookAheadE . liftM distribute . m)
    where
    distribute (Left a, s') = Left (Left a, s')
    distribute (Right b, s') = Right (Right b, s')
    factor = either id id
  {-# INLINE lookAheadE #-}

instance MonadGet m => MonadGet (ReaderT e m) where
  type Unchecked (ReaderT e m) = Unchecked m
  type Bytes (ReaderT e m) = Bytes m
  lookAhead (ReaderT m) = ReaderT (lookAhead . m)
  {-# INLINE lookAhead #-}
  lookAheadM (ReaderT m) = ReaderT (lookAheadM . m)
  {-# INLINE lookAheadM #-}
  lookAheadE (ReaderT m) = ReaderT (lookAheadE . m)
  {-# INLINE lookAheadE #-}

instance (MonadGet m, Monoid w) => MonadGet (Lazy.WriterT w m) where
  type Unchecked (Lazy.WriterT w m) = Unchecked m
  type Bytes (Lazy.WriterT w m) = Bytes m
  lookAhead (Lazy.WriterT m) = Lazy.WriterT (lookAhead m)
  {-# INLINE lookAhead #-}
  lookAheadM (Lazy.WriterT m) = Lazy.WriterT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Nothing, s') = Left (Nothing, s')
    distribute (Just a, s') = Right (Just a, s')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Lazy.WriterT m) = Lazy.WriterT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Left a, s') = Left (Left a, s')
    distribute (Right b, s') = Right (Right b, s')
    factor = either id id
  {-# INLINE lookAheadE #-}

instance (MonadGet m, Monoid w) => MonadGet (Strict.WriterT w m) where
  type Unchecked (Strict.WriterT w m) = Unchecked m
  type Bytes (Strict.WriterT w m) = Bytes m
  lookAhead (Strict.WriterT m) = Strict.WriterT (lookAhead m)
  {-# INLINE lookAhead #-}
  lookAheadM (Strict.WriterT m) = Strict.WriterT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Nothing, s') = Left (Nothing, s')
    distribute (Just a, s') = Right (Just a, s')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Strict.WriterT m) = Strict.WriterT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Left a, s') = Left (Left a, s')
    distribute (Right b, s') = Right (Right b, s')
    factor = either id id
  {-# INLINE lookAheadE #-}
