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
  -- * Lenses
  , globalDigest
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

data Global = Global
  { _globalDigest   :: {-# UNPACK #-} !Digest
  , _globalPackage  :: !ByteString
  , _globalModule   :: !ByteString
  , _globalName     :: !ByteString
  } deriving (Show, Data, Typeable)

globalDigest :: Getter Global Digest
globalDigest = to _globalDigest
globalPackage, globalModule, globalName :: Simple Lens Global ByteString
globalPackage f (Global _ p m n) = (\p' -> global p' m n) <$> f p
globalModule f (Global _ p m n) = (\m' -> global p m' n) <$> f m
globalName f (Global _ p m n) = global p m <$> f n

instance Eq Global where
  a == b = _globalDigest a == _globalDigest b

instance Ord Global where
  compare a b = _globalDigest a `compare` _globalDigest b

instance MD5 Global where
  digest = _globalDigest

instance Hashable Global where
  hashWithSalt s c = hashWithSalt s (_globalDigest c)

global :: ByteString -> ByteString -> ByteString -> Global
global p m n = Global (digest (Char8.unwords [p, m, n])) p m n
