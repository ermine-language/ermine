{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2011
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--
--------------------------------------------------------------------
module Ermine.Syntax.Global
  ( Global(Global)
  , HasGlobal(..)
  , AsGlobal(..)
  , glob
  , Assoc(..)
  , Fixity(..)
  , fixity
  -- * Lenses
  , unpackFixity
  , packFixity
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Crypto.Classes
import Crypto.Hash.MD5 as MD5
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.Bits
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.ByteString
import Data.Data (Data, Typeable)
import Data.Function (on)
import Data.Hashable
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Data.Word
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

-- | Pack 'Fixity' into a 'Word8'.
--
-- Format:
--
-- >  01234567
-- >  ccaapppp
--
-- @cc@ is constructor tag, @0-3@
-- @pppp@ is precedence level, @0-9@
-- @aa@ is associativity tag, @0-2@
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

-- this should be MonadPlus, but Get isn't
unpackFixity :: Monad m => Word8 -> m Fixity
unpackFixity w8 =
  case 0xC0 .&. w8 of
    0x00 -> case 0x30 .&. w8 of
      0x00 -> return $ Infix L n
      0x10 -> return $ Infix R n
      0x20 -> return $ Infix N n
      _    -> fail "unpackFixity: bad associativity"
    0x40 -> return $ Prefix n
    0x80 -> return $ Postfix n
    0xC0 -> return Idfix
    _    -> fail "unpackFixity: IMPOSSIBLE"
 where n = fromIntegral $ 0x0F .&. w8
{-# INLINE unpackFixity #-}

fixity :: Prism' Word8 Fixity
fixity = prism' packFixity unpackFixity
{-# INLINE fixity #-}

instance Serial Fixity where
  serialize f = putWord8 $ packFixity f
  deserialize = do
    w <- getWord8
    unpackFixity w

instance Binary Fixity where
  put = serialize
  get = deserialize

instance Serialize Fixity where
  put = serialize
  get = deserialize

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

instance Serial Global where
  serialize (Global d f p m n) = serialize d >> serialize f >> serialize p >> serialize m >> serialize n
  deserialize = liftM5 Global deserialize deserialize deserialize deserialize deserialize

instance Binary Global where
  put = serialize
  get = deserialize

class HasGlobal t where
  global :: Lens' t Global
  -- | A lens that will read or update the fixity (and compute a new digest)
  globalFixity :: Lens' t Fixity
  globalFixity f = global $ \ (Global _ a p m n) -> (\a' -> glob a' p m n) <$> f a
  -- | A lenses that will read or update part of the Global and compute a new digest as necessary.
  globalPackage, globalModule, globalName :: Lens' t ByteString
  globalPackage f = global $ \ (Global _ a p m n) -> (\p' -> glob a p' m n) <$> f p
  globalModule f = global $ \ (Global _ a p m n) -> (\m' -> glob a p m' n) <$> f m
  globalName f = global $ \ (Global _ a p m n) -> glob a p m <$> f n

instance HasGlobal Global where
  global = id

instance Eq Global where
  (==) = (==) `on` _globalDigest

instance Ord Global where
  compare = compare `on` _globalDigest

instance Digestable Global where
  digest c = digest c . _globalDigest

instance Hashable Global where
  hashWithSalt s c = hashWithSalt s (_globalDigest c)

class AsGlobal t where
  _Global :: Prism' t Global

instance AsGlobal Global where
  _Global = id

-- | Construct a 'Global' with a correct digest.
glob :: AsGlobal t => Fixity -> ByteString -> ByteString -> ByteString -> t
glob f p m n = _Global # Global d f p m n where
  d = MD5.finalize (digest (digest initialCtx f) [p,m,n])
