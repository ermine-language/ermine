{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Syntax.Global
-- Copyright :  (c) Edward Kmett 2011
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--
--------------------------------------------------------------------
module Ermine.Syntax.Global
  ( Global(Global)
  , AsGlobal(..)
  , glob
  , Assoc(..)
  , Fixity(..)
  -- * Lenses
  , globalFixity
  , globalPackage
  , globalModule
  , globalName
  , unpackFixity
  , packFixity
  ) where

import Control.Applicative
import Control.Lens
import Crypto.Classes
import Crypto.Hash.MD5 as MD5
import Data.Binary
import Data.Bits
import Data.ByteString
import Data.Data (Data, Typeable)
import Data.Function (on)
import Data.Hashable
import Ermine.Syntax.Digest

-- | The associativity of an infix identifier
data Assoc = L | R | N deriving (Eq,Ord,Show,Read,Enum,Data,Typeable)

-- | The fixity of an identifier
data Fixity
  = Infix !Assoc !Int
  | Prefix !Int
  | Postfix !Int
  | Idfix
  deriving (Eq,Ord,Show,Read,Data,Typeable)

-- | Packs fixity info into a Word8.
--
-- Format:
-- >  01234567
-- >  ccaapppp
-- cc is constructor tag, 0-3
-- pppp is precedence level, 0-9
-- aa is associativity tag, 0-2
packFixity :: Fixity -> Word8
packFixity Idfix       = 0xC0
packFixity (Prefix  n) = 0x40 .|. (0x0F .&. fromIntegral n)
packFixity (Postfix n) = 0x80 .|. (0x0F .&. fromIntegral n)
packFixity (Infix a n) = packAssoc a .|. (0x0F .&. fromIntegral n)
 where
 packAssoc L = 0x00
 packAssoc R = 0x10
 packAssoc N = 0x20
{-# INLINE packFixity #-}

unpackFixity :: Word8 -> Fixity
unpackFixity w8 = case 0xC0 .&. w8 of
                    0x00 -> Infix a n
                    0x40 -> Prefix n
                    0x80 -> Postfix n
                    0xC0 -> Idfix
                    _    -> error "unpackFixity: IMPOSSIBLE"
 where
 n = fromIntegral $ 0x0F .&. w8
 a = case 0x30 .&. w8 of 0x00 -> L ; 0x10 -> R ; 0x20 -> N
                         _ -> error "unpackFixity: bad associativity"
{-# INLINE unpackFixity #-}

instance Binary Fixity where
  put f = putWord8 $ packFixity f
  get = unpackFixity <$> getWord8

instance Digestable Assoc
instance Digestable Fixity

-- | A 'Global' is a full qualified top level name.
--
-- /NB:/ You should construct these with 'global' and only use the constructor for pattern matching.
data Global = Global
  { _globalDigest   :: !ByteString
  , _globalFixity   :: !Fixity
  , _globalPackage  :: !ByteString
  , _globalModule   :: !ByteString
  , _globalName     :: !ByteString
  } deriving (Data, Typeable)

instance Show Global where
  showsPrec d (Global _ f p m n) = showParen (d > 10) $
    showString "global " . showsPrec 11 f .
            showChar ' ' . showsPrec 11 p .
            showChar ' ' . showsPrec 11 m .
            showChar ' ' . showsPrec 11 n

instance Binary Global where
  put (Global d f p m n) = put d *> put f *> put p *> put m *> put n
  get = Global <$> get <*> get <*> get <*> get <*> get

-- | A lens that will read or update the fixity (and compute a new digest)
globalFixity :: Simple Lens Global Fixity
globalFixity f (Global _ a p m n) = (\a' -> glob a' p m n) <$> f a

-- | A lenses that will read or update part of the Global and compute a new digest as necessary.
globalPackage, globalModule, globalName :: Simple Lens Global ByteString
globalPackage f (Global _ a p m n) = (\p' -> glob a p' m n) <$> f p
globalModule f (Global _ a p m n) = (\m' -> glob a p m' n) <$> f m
globalName f (Global _ a p m n) = glob a p m <$> f n

instance Eq Global where
  (==) = (==) `on` _globalDigest

instance Ord Global where
  compare = compare `on` _globalDigest

instance Digestable Global where
  digest c = digest c . _globalDigest

instance Hashable Global where
  hashWithSalt s c = hashWithSalt s (_globalDigest c)

class AsGlobal t where
  -- global :: Prism' t Global

-- | Construct a 'Global' with a correct digest.
glob :: Fixity -> ByteString -> ByteString -> ByteString -> Global
glob f p m n = Global d f p m n where
  d = MD5.finalize (digest (digest initialCtx f) [p,m,n])
