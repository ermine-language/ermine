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
  , global
  , Assoc(..)
  , Fixity(..)
  -- * Lenses
  , globalFixity
  , globalPackage
  , globalModule
  , globalName
  ) where

import Control.Applicative
import Control.Lens
import Crypto.Classes
import Crypto.Hash.MD5 as MD5
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

-- | A lens that will read or update the fixity (and compute a new digest)
globalFixity :: Simple Lens Global Fixity
globalFixity f (Global _ a p m n) = (\a' -> global a' p m n) <$> f a

-- | A lenses that will read or update part of the Global and compute a new digest as necessary.
globalPackage, globalModule, globalName :: Simple Lens Global ByteString
globalPackage f (Global _ a p m n) = (\p' -> global a p' m n) <$> f p
globalModule f (Global _ a p m n) = (\m' -> global a p m' n) <$> f m
globalName f (Global _ a p m n) = global a p m <$> f n

instance Eq Global where
  (==) = (==) `on` _globalDigest

instance Ord Global where
  compare = compare `on` _globalDigest

instance Digestable Global where
  digest c = digest c . _globalDigest

instance Hashable Global where
  hashWithSalt s c = hashWithSalt s (_globalDigest c)

-- | Construct a 'Global' with a correct digest.
global :: Fixity -> ByteString -> ByteString -> ByteString -> Global
global f p m n = Global d f p m n where
  d = MD5.finalize (digest (digest initialCtx f) [p,m,n])
