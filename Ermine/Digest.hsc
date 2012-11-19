{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Digest
-- Copyright :  (c) Edward Kmett 2011-12
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  provisional
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Digest
  ( Digest(..)
  , MD5(..)
  , MD5List(..)
  ) where

import Control.Applicative
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

data Digest = Digest {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Show, Eq, Ord, Data, Typeable)

instance Default Digest where
  def = Digest 0 0

instance Hashable Digest where
  hashWithSalt s (Digest hi lo) = s `hashWithSalt` hi `hashWithSalt` lo

#include "openssl/md5.h"

data Ctx
foreign import ccall unsafe "openssl/md5.h MD5Init"
  md5Init :: Ptr Ctx -> IO ()
foreign import ccall unsafe "openssl/md5.h MD5Update"
  md5Update :: Ptr Ctx -> Ptr Word8 -> CInt -> IO ()
foreign import ccall unsafe "openssl/md5.h MD5Final"
  md5Final :: Ptr Word8 -> Ptr Ctx -> IO ()

peekDigest :: Ptr Word8 -> IO Digest
peekDigest p = Digest <$> peek64 p 8 0 <*> peek64 (plusPtr p 8) 8 0 where

peek64 :: Ptr Word8 -> Int -> Word64 -> IO Word64
peek64 !_ 0 !i = return i
peek64 p n i = do
  w8 <- peek p
  peek64 (plusPtr p 1) (n-1) (shiftL i 8 .|. fromIntegral w8)

withCtx :: (Ptr Ctx -> IO a) -> Digest
withCtx f = unsafeDupablePerformIO $ allocaBytes (#const sizeof(MD5_CTX)) $ \ctx -> do
  md5Init ctx
  _ <- f ctx
  allocaBytes 16 $ \pdigest -> do
    md5Final pdigest ctx
    peekDigest (castPtr pdigest)

class MD5List t where
  digestList :: [t] -> Digest

instance MD5List Strict.ByteString where
  digestList b = withCtx $ \c -> forM_ b $ \x ->
    Strict.unsafeUseAsCStringLen x $ \(p, n) -> md5Update c (castPtr p) (fromIntegral n)

instance MD5List Char where
  digestList s = withCtx $ \ctx ->
    withArrayLen (concatMap f s) $ \n p ->
      md5Update ctx p (fromIntegral n)
    where
      f c = [0, fromIntegral $ shiftR w32 16, fromIntegral $ shiftR w32 8, fromIntegral w32] where
        w32 :: Word32
        w32 = fromIntegral (fromEnum c)

class MD5 t where
  digest :: t -> Digest

instance MD5List t => MD5 [t] where
  digest = digestList

instance MD5 Lazy.ByteString where
  digest b = digestList (Lazy.toChunks b)

instance MD5 () where
  digest _ = Digest 0 0

instance MD5 Digest where
  digest d = d

instance MD5 Strict.ByteString where
  digest b = withCtx $ \c -> Strict.unsafeUseAsCStringLen b $ \(p,n) ->
    md5Update c (castPtr p) (fromIntegral n)

instance MD5 Builder where
  digest b = digest (toLazyByteString b)
