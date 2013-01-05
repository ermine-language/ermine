{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Global
-- Copyright :  (c) Edward Kmett 2011
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--
--------------------------------------------------------------------
module Ermine.Global
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
import Data.Data (Data, Typeable)
import Data.Hashable
import Data.ByteString
import Data.ByteString.Char8 as Char8
import Ermine.Digest

-- | The associativity of an infix identifier
data Assoc = L | R | N deriving (Eq,Ord,Show,Read,Enum,Data,Typeable)

-- | The fixity of an identifier
data Fixity
  = Infix !Assoc !Int
  | Prefix !Int
  | Postfix !Int
  | Idfix
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance MD5 Assoc
instance MD5 Fixity

-- | A 'Global' is a full qualified top level name.
--
-- /NB:/ You should construct these with 'global' and only use the constructor for pattern matching.
data Global = Global
  { _globalDigest   :: !Digest
  , _globalFixity   :: !Fixity
  , _globalPackage  :: !ByteString
  , _globalModule   :: !ByteString
  , _globalName     :: !ByteString
  } deriving (Show, Data, Typeable)

-- | A lens that will read or update the fixity (and compute a new digest)
globalFixity :: Simple Lens Global Fixity
globalFixity f (Global _ a p m n) = (\a' -> global a' p m n) <$> f a

-- | A lenses that will read or update part of the Global and compute a new digest as necessary.
globalPackage, globalModule, globalName :: Simple Lens Global ByteString
globalPackage f (Global _ a p m n) = (\p' -> global a p' m n) <$> f p
globalModule f (Global _ a p m n) = (\m' -> global a p m' n) <$> f m
globalName f (Global _ a p m n) = global a p m <$> f n

instance Eq Global where
  a == b = digest a == digest b

instance Ord Global where
  compare a b = digest a `compare` digest b

instance MD5 Global where
  digest = _globalDigest

instance Hashable Global where
  hashWithSalt s c = hashWithSalt s (digest c)

-- | Construct a 'Global' with a correct digest.
global :: Fixity -> ByteString -> ByteString -> ByteString -> Global
global f p m n = Global (digest (Char8.unwords [Char8.pack (show f), p, m, n])) f p m n
